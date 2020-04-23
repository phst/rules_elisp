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

#include <fstream>
#include <iterator>
#include <iostream>
#include <string>
#include <string_view>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#include "absl/strings/str_cat.h"
#include "absl/types/optional.h"
#pragma GCC diagnostic pop

#include "internal/random.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace phst_rules_elisp {
namespace {

using ::testing::TempDir;
using ::testing::Eq;
using ::testing::Ge;
using ::testing::StartsWith;
using ::testing::EndsWith;

static std::string read_file(const std::string& path) try {
  std::ifstream stream(path);
  stream.exceptions(stream.badbit | stream.failbit | stream.eofbit);
  using iterator = std::istreambuf_iterator<char>;
  return std::string(iterator(stream), iterator());
} catch (const std::ios::failure& ex) {
  std::clog << "error reading file " << path << ": " << ex.what() << std::endl;
  throw;
}

TEST(File, WriteRead) {
  const auto path = join_path(TempDir(), random().temp_name("file-*.tmp"));
  using traits = std::char_traits<char>;
  {
    file file(path, file_mode::readwrite | file_mode::create | file_mode::excl);
    EXPECT_THAT(file.path(), Eq(path));
    EXPECT_THAT(file.sputc('h'), traits::to_int_type('h'));
    EXPECT_THAT(file.sputn("ello worl", 9), Eq(9));
    EXPECT_THAT(file.pubsync(), Eq(0));
    EXPECT_THAT(file.sputc('d'), traits::to_int_type('d'));
    file.close();
  }
  EXPECT_THAT(read_file(path), "hello world");
  {
    file file(path, file_mode::read);
    EXPECT_THAT(file.path(), Eq(path));
    EXPECT_THAT(file.sbumpc(), Eq(traits::to_int_type('h')));
    EXPECT_THAT(file.sgetc(), Eq(traits::to_int_type('e')));
    EXPECT_THAT(file.sbumpc(), Eq(traits::to_int_type('e')));
    EXPECT_THAT(file.sgetc(), Eq(traits::to_int_type('l')));
    EXPECT_THAT(file.snextc(), Eq(traits::to_int_type('l')));
    EXPECT_THAT(file.snextc(), Eq(traits::to_int_type('o')));
    std::array<char, 10> buffer;
    EXPECT_THAT(file.sgetn(buffer.data(), buffer.size()), 7);
    EXPECT_THAT(std::string(buffer.cbegin(), buffer.cbegin() + 7), "o world");
    EXPECT_THAT(file.sgetc(), Eq(traits::eof()));
    EXPECT_THAT(file.snextc(), Eq(traits::eof()));
    EXPECT_THAT(file.sbumpc(), Eq(traits::eof()));
    EXPECT_THAT(file.pubsync(), Eq(0));
    file.close();
  }
}

TEST(File, PartialWrite) {
  using traits = std::char_traits<char>;
  const int fd = memfd_create("test", MFD_CLOEXEC | MFD_ALLOW_SEALING);
  ASSERT_THAT(fd, Ge(0)) << std::error_code(errno, std::system_category());
  const struct closer {
    ~closer() noexcept { ::close(fd); }
    int fd;
  } closer{fd};
  EXPECT_THAT(ftruncate(fd, 1), Eq(0))
      << std::error_code(errno, std::system_category());
  EXPECT_THAT(fcntl(fd, F_ADD_SEALS, F_SEAL_GROW), Eq(0))
      << std::error_code(errno, std::system_category());
  file file(absl::StrCat("/dev/fd/", fd), file_mode::write);
  EXPECT_THAT(file.sputc('h'), Eq(traits::to_int_type('h')));  // fill buffer
  EXPECT_THAT(file.sputn("i", 1), Eq(0));  // try to flush
  file.close();  // buffer flushed, no new error
  std::array<char, 20> buffer;
  EXPECT_THAT(::read(fd, buffer.data(), buffer.size()), Eq(1));
  EXPECT_THAT(::close(fd), Eq(0))
      << std::error_code(errno, std::system_category());
  EXPECT_THAT(buffer.front(), Eq('h'));
}

TEST(File, Move) {
  // This test should typically be run under Address Sanitizer to debug issues
  // with the move constructor.
  const auto path = join_path(TempDir(), random().temp_name("file-*.tmp"));
  using traits = std::char_traits<char>;
  absl::optional<file> outer;
  {
    file inner(path, file_mode::write | file_mode::create | file_mode::excl);
    EXPECT_THAT(inner.sputc('h'), Eq(traits::to_int_type('h')));
    outer = std::move(inner);
  }
  EXPECT_THAT(outer->sputc('i'), Eq(traits::to_int_type('i')));
  outer->close();
  EXPECT_THAT(read_file(path), "hi");
  {
    file inner(path, file_mode::read);
    EXPECT_THAT(inner.sgetc(), Eq(traits::to_int_type('h')));
    char ch;
    EXPECT_THAT(inner.sgetn(&ch, 1), Eq(1));
    EXPECT_THAT(ch, Eq('h'));
    outer = std::move(inner);
  }
  EXPECT_THAT(outer->sgetc(), Eq(traits::to_int_type('i')));
  EXPECT_THAT(outer->sbumpc(), Eq(traits::to_int_type('i')));
  EXPECT_THAT(outer->sbumpc(), Eq(traits::eof()));
}

TEST(TempFile, Create) {
  random rnd;
  temp_file file(TempDir(), "foo-*.tmp", rnd);
  const auto path = file.path();
  EXPECT_THAT(parent(path), Eq(remove_slash(TempDir())));
  EXPECT_THAT(std::string(filename(path)), StartsWith("foo-"));
  EXPECT_THAT(std::string(filename(path)), EndsWith(".tmp"));
  EXPECT_TRUE(file_exists(path));
  file.close();
  EXPECT_TRUE(file.path().empty());
  EXPECT_FALSE(file_exists(path));
}

TEST(TempStream, Format) {
  random rnd;
  temp_stream stream(TempDir(), "foo-*.tmp", rnd);
  const auto path = stream.path();
  EXPECT_THAT(parent(path), Eq(remove_slash(TempDir())));
  EXPECT_THAT(std::string(filename(path)), StartsWith("foo-"));
  EXPECT_THAT(std::string(filename(path)), EndsWith(".tmp"));
  EXPECT_TRUE(file_exists(path));
  stream << "hello world\n" << 123;
  stream.flush();
  EXPECT_THAT(read_file(path), "hello world\n123");
  stream.close();
  EXPECT_TRUE(stream.path().empty());
  EXPECT_FALSE(file_exists(path));
}

}  // namespace
}  // namespace phst_rules_elisp
