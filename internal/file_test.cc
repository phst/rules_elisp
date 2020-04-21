// Copyright 2020 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     https://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include "internal/file.h"

#include <sys/mman.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>

#include <cstdlib>
#include <fstream>
#include <iterator>
#include <iostream>
#include <string>
#include <string_view>
#include <utility>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#include "absl/status/status.h"
#include "absl/strings/str_cat.h"
#include "absl/types/optional.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#pragma GCC diagnostic pop

#include "internal/random.h"

namespace phst_rules_elisp {
namespace {

using ::testing::TempDir;
using ::testing::Eq;
using ::testing::Ge;
using ::testing::StrEq;
using ::testing::StartsWith;
using ::testing::EndsWith;

static absl::string_view FileName(absl::string_view name) noexcept {
  const auto pos = name.rfind('/');
  return pos == name.npos ? name : name.substr(pos + 1);
}

static absl::string_view Parent(absl::string_view name) noexcept {
  const auto pos = name.rfind('/');
  return pos == name.npos ? absl::string_view() : name.substr(0, pos);
}

static constexpr absl::string_view RemoveSlash(
    absl::string_view name) noexcept {
  return (name.empty() || name.back() != '/') ? name
                                              : name.substr(0, name.size() - 1);
}

static std::string ReadFile(const std::string& path) {
  std::ifstream stream(path);
  stream.exceptions(stream.badbit | stream.failbit | stream.eofbit);
  using iterator = std::istreambuf_iterator<char>;
  return std::string(iterator(stream), iterator());
}

TEST(File, WriteRead) {
  const auto path = JoinPath(TempDir(), Random().TempName("file-*.tmp"));
  using Traits = std::char_traits<char>;
  {
    auto status_or_file = File::Open(
        path, FileMode::kReadWrite | FileMode::kCreate | FileMode::kExclusive);
    ASSERT_TRUE(status_or_file.ok()) << status_or_file.status();
    auto& file = status_or_file.value();
    EXPECT_THAT(file.path(), Eq(path));
    EXPECT_THAT(file.sputc('h'), Traits::to_int_type('h'));
    EXPECT_THAT(file.sputn("ello worl", 9), Eq(9));
    EXPECT_THAT(file.pubsync(), Eq(0));
    EXPECT_THAT(file.sputc('d'), Traits::to_int_type('d'));
    EXPECT_TRUE(file.Close().ok());
  }
  EXPECT_THAT(ReadFile(path), "hello world");
  {
    auto status_or_file = File::Open(path, FileMode::kRead);
    ASSERT_TRUE(status_or_file.ok()) << status_or_file.status();
    auto& file = status_or_file.value();
    EXPECT_THAT(file.path(), Eq(path));
    EXPECT_THAT(file.sbumpc(), Eq(Traits::to_int_type('h')));
    EXPECT_THAT(file.sgetc(), Eq(Traits::to_int_type('e')));
    EXPECT_THAT(file.sbumpc(), Eq(Traits::to_int_type('e')));
    EXPECT_THAT(file.sgetc(), Eq(Traits::to_int_type('l')));
    EXPECT_THAT(file.snextc(), Eq(Traits::to_int_type('l')));
    EXPECT_THAT(file.snextc(), Eq(Traits::to_int_type('o')));
    std::array<char, 10> buffer;
    EXPECT_THAT(file.sgetn(buffer.data(), buffer.size()), 7);
    EXPECT_THAT(std::string(buffer.cbegin(), buffer.cbegin() + 7), "o world");
    EXPECT_THAT(file.sgetc(), Eq(Traits::eof()));
    EXPECT_THAT(file.snextc(), Eq(Traits::eof()));
    EXPECT_THAT(file.sbumpc(), Eq(Traits::eof()));
    EXPECT_THAT(file.pubsync(), Eq(0));
    EXPECT_TRUE(file.Close().ok());
  }
}

TEST(File, PartialWrite) {
  using Traits = std::char_traits<char>;
  const int fd = memfd_create("test", MFD_CLOEXEC | MFD_ALLOW_SEALING);
  ASSERT_THAT(fd, Ge(0)) << std::error_code(errno, std::system_category());
  const struct Closer {
    ~Closer() noexcept { ::close(fd); }
    int fd;
  } closer{fd};
  EXPECT_THAT(ftruncate(fd, 1), Eq(0))
      << std::error_code(errno, std::system_category());
  EXPECT_THAT(fcntl(fd, F_ADD_SEALS, F_SEAL_GROW), Eq(0))
      << std::error_code(errno, std::system_category());
  auto status_or_file =
      File::Open(absl::StrCat("/dev/fd/", fd), FileMode::kWrite);
  ASSERT_TRUE(status_or_file.ok()) << status_or_file.status();
  auto& file = status_or_file.value();
  EXPECT_THAT(file.sputc('h'), Eq(Traits::to_int_type('h')));  // fill buffer
  EXPECT_THAT(file.sputn("i", 1), Eq(0));  // try to flush
  EXPECT_TRUE(file.Close().ok());  // buffer flushed, no new error
  std::array<char, 20> buffer;
  EXPECT_THAT(::read(fd, buffer.data(), buffer.size()), Eq(1));
  EXPECT_THAT(::close(fd), Eq(0))
      << std::error_code(errno, std::system_category());
  EXPECT_THAT(buffer.front(), Eq('h'));
}

TEST(File, Move) {
  // This test should typically be run under Address Sanitizer to debug issues
  // with the move constructor.
  const auto path = JoinPath(TempDir(), Random().TempName("file-*.tmp"));
  using Traits = std::char_traits<char>;
  absl::optional<File> outer;
  {
    auto status_or_file = File::Open(
        path, FileMode::kWrite | FileMode::kCreate | FileMode::kExclusive);
    ASSERT_TRUE(status_or_file.ok()) << status_or_file.status();
    auto& inner = status_or_file.value();
    EXPECT_THAT(inner.sputc('h'), Eq(Traits::to_int_type('h')));
    outer = std::move(inner);
  }
  EXPECT_THAT(outer->sputc('i'), Eq(Traits::to_int_type('i')));
  EXPECT_TRUE(outer->Close().ok());
  EXPECT_THAT(ReadFile(path), "hi");
  {
    auto status_or_file = File::Open(path, FileMode::kRead);
    ASSERT_TRUE(status_or_file.ok()) << status_or_file.status();
    auto& inner = status_or_file.value();
    EXPECT_THAT(inner.sgetc(), Eq(Traits::to_int_type('h')));
    char ch;
    EXPECT_THAT(inner.sgetn(&ch, 1), Eq(1));
    EXPECT_THAT(ch, Eq('h'));
    outer = std::move(inner);
  }
  EXPECT_THAT(outer->sgetc(), Eq(Traits::to_int_type('i')));
  EXPECT_THAT(outer->sbumpc(), Eq(Traits::to_int_type('i')));
  EXPECT_THAT(outer->sbumpc(), Eq(Traits::eof()));
}

TEST(TempFile, Create) {
  Random rnd;
  auto status_or_file = TempFile::Open(TempDir(), "foo-*.tmp", rnd);
  ASSERT_TRUE(status_or_file.ok()) << status_or_file.status();
  auto& file = status_or_file.value();
  const auto path = file.path();
  EXPECT_THAT(Parent(path), Eq(RemoveSlash(TempDir())));
  EXPECT_THAT(std::string(FileName(path)), StartsWith("foo-"));
  EXPECT_THAT(std::string(FileName(path)), EndsWith(".tmp"));
  EXPECT_TRUE(FileExists(path));
  EXPECT_TRUE(file.Close().ok());
  EXPECT_TRUE(file.path().empty());
  EXPECT_FALSE(FileExists(path));
}

TEST(Stream, AssignRead) {
  auto status_or_file =
      Stream::Open(JoinPath(std::getenv("TEST_SRCDIR"),
                            "phst_rules_elisp/internal/test.txt"),
                   FileMode::kRead);
  ASSERT_TRUE(status_or_file.ok()) << status_or_file.status();
  auto a = std::move(status_or_file).value();
  auto b = std::move(a);
  using Iterator = std::istreambuf_iterator<char>;
  std::string contents(Iterator(b), Iterator{});
  EXPECT_THAT(contents, StrEq("hi\n"));
  std::swap(a, b);
}

TEST(TempStream, Format) {
  Random rnd;
  auto status_or_stream = TempStream::Open(TempDir(), "foo-*.tmp", rnd);
  ASSERT_TRUE(status_or_stream.ok()) << status_or_stream.status();
  auto& stream = status_or_stream.value();
  const auto path = stream.path();
  EXPECT_THAT(Parent(path), Eq(RemoveSlash(TempDir())));
  EXPECT_THAT(std::string(FileName(path)), StartsWith("foo-"));
  EXPECT_THAT(std::string(FileName(path)), EndsWith(".tmp"));
  EXPECT_TRUE(FileExists(path));
  stream << "hello world\n" << 123;
  stream.flush();
  EXPECT_THAT(ReadFile(path), "hello world\n123");
  EXPECT_TRUE(stream.Close().ok());
  EXPECT_TRUE(stream.path().empty());
  EXPECT_FALSE(FileExists(path));
}

TEST(JoinPath, Relative) {
  EXPECT_THAT(JoinPath("foo/", "/bar/", "baz/qux/"), StrEq("foo/bar/baz/qux/"));
}

TEST(JoinPath, Absolute) {
  EXPECT_THAT(JoinPath("/foo/", "/bar/", "baz/qux/"),
              StrEq("/foo/bar/baz/qux/"));
}

TEST(JoinPath, TwoPieces) {
  EXPECT_THAT(JoinPath("/foo/", "/bar/"), StrEq("/foo/bar/"));
}

}  // namespace
}  // namespace phst_rules_elisp
