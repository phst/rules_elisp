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
#include <string>
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
using ::testing::IsFalse;
using ::testing::IsTrue;
using ::testing::Eq;
using ::testing::Ge;
using ::testing::StrEq;
using ::testing::StartsWith;
using ::testing::EndsWith;
using ::testing::IsEmpty;

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
  {
    auto status_or_file = File::Open(
        path, FileMode::kReadWrite | FileMode::kCreate | FileMode::kExclusive);
    ASSERT_TRUE(status_or_file.ok()) << status_or_file.status();
    auto& file = status_or_file.value();
    EXPECT_THAT(file.path(), Eq(path));
    EXPECT_THAT(file.Write("hello world").ok(), IsTrue());
    EXPECT_TRUE(file.Close().ok());
  }
  EXPECT_THAT(ReadFile(path), "hello world");
  {
    auto status_or_file = File::Open(path, FileMode::kRead);
    ASSERT_TRUE(status_or_file.ok()) << status_or_file.status();
    auto& file = status_or_file.value();
    EXPECT_THAT(file.path(), Eq(path));
    const auto status_or_contents = file.Read();
    ASSERT_THAT(status_or_contents.ok(), IsTrue());
    EXPECT_THAT(status_or_contents.value(), StrEq("hello world"));
    EXPECT_TRUE(file.Close().ok());
  }
}

#if defined MFD_CLOEXEC && defined MFD_ALLOW_SEALING \
  && defined F_ADD_SEALS && defined F_SEAL_GROW
TEST(File, PartialWrite) {
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
  const auto status = file.Write("h");
  EXPECT_THAT(status.ok(), IsTrue()) << status;
  EXPECT_THAT(file.Write("i").ok(), IsFalse());
  EXPECT_TRUE(file.Close().ok());  // buffer flushed, no new error
  std::array<char, 20> buffer;
  EXPECT_THAT(::read(fd, buffer.data(), buffer.size()), Eq(1));
  EXPECT_THAT(::close(fd), Eq(0))
      << std::error_code(errno, std::system_category());
  EXPECT_THAT(buffer.front(), Eq('h'));
}
#endif

TEST(File, Move) {
  // This test should typically be run under Address Sanitizer to debug issues
  // with the move constructor.
  const auto path = JoinPath(TempDir(), Random().TempName("file-*.tmp"));
  absl::optional<File> outer;
  {
    auto status_or_file = File::Open(
        path, FileMode::kWrite | FileMode::kCreate | FileMode::kExclusive);
    ASSERT_TRUE(status_or_file.ok()) << status_or_file.status();
    auto& inner = status_or_file.value();
    const auto status = inner.Write("h");
    EXPECT_THAT(status.ok(), IsTrue()) << status;;
    outer = std::move(inner);
  }
  const auto status = outer->Write("i");
  EXPECT_THAT(status.ok(), IsTrue()) << status;
  EXPECT_TRUE(outer->Close().ok());
  EXPECT_THAT(ReadFile(path), "hi");
  {
    auto status_or_file = File::Open(path, FileMode::kRead);
    ASSERT_TRUE(status_or_file.ok()) << status_or_file.status();
    auto& inner = status_or_file.value();
    const auto status_or_contents = inner.Read();
    ASSERT_THAT(status_or_contents.ok(), IsTrue());
    EXPECT_THAT(status_or_contents.value(), StrEq("hi"));
    outer = std::move(inner);
  }
  const auto status_or_contents = outer->Read();
  ASSERT_THAT(status_or_contents.ok(), IsTrue());
  EXPECT_THAT(status_or_contents.value(), IsEmpty());
  EXPECT_TRUE(outer->Close().ok());
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

TEST(File, AssignRead) {
  auto status_or_file =
      File::Open(JoinPath(std::getenv("TEST_SRCDIR"),
                          "phst_rules_elisp/internal/test.txt"),
                 FileMode::kRead);
  ASSERT_TRUE(status_or_file.ok()) << status_or_file.status();
  auto a = std::move(status_or_file).value();
  auto b = std::move(a);
  const auto status_or_contents = b.Read();
  ASSERT_THAT(status_or_contents.ok(), IsTrue());
  EXPECT_THAT(status_or_contents.value(), StrEq("hi\n"));
  std::swap(a, b);
  EXPECT_TRUE(a.Close().ok());
}

TEST(TempFile, Write) {
  Random rnd;
  auto status_or_stream = TempFile::Open(TempDir(), "foo-*.tmp", rnd);
  ASSERT_TRUE(status_or_stream.ok()) << status_or_stream.status();
  auto& stream = status_or_stream.value();
  const auto path = stream.path();
  EXPECT_THAT(Parent(path), Eq(RemoveSlash(TempDir())));
  EXPECT_THAT(std::string(FileName(path)), StartsWith("foo-"));
  EXPECT_THAT(std::string(FileName(path)), EndsWith(".tmp"));
  EXPECT_TRUE(FileExists(path));
  const auto status = stream.Write("hello world\n");
  EXPECT_THAT(status.ok(), IsTrue()) << status;
  EXPECT_THAT(ReadFile(path), "hello world\n");
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
