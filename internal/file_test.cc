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

#include <fstream>
#include <iterator>
#include <iostream>
#include <string>
#include <string_view>

#include "internal/random.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace phst_rules_elisp {
namespace {

using ::testing::TempDir;
using ::testing::Eq;
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
  using traits = std::char_traits<char>;
  const auto path = join_path(TempDir(), "file.tmp");
  const auto code = remove_file(path);
  if (code &&
      code != std::make_error_condition(std::errc::no_such_file_or_directory)) {
    throw std::system_error(code);
  }
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
