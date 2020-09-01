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

#include "elisp/file.h"

#include <fstream>
#include <iterator>
#include <string>
#include <utility>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#include "absl/status/status.h"
#include "absl/strings/string_view.h"
#include "absl/types/optional.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#pragma GCC diagnostic pop

namespace phst_rules_elisp {
namespace {

using ::testing::TempDir;
using ::testing::Not;
using ::testing::IsFalse;
using ::testing::IsTrue;
using ::testing::Eq;
using ::testing::Ge;
using ::testing::StrEq;
using ::testing::StrNe;
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

static absl::Status GetStatus(const absl::Status& status) {
  return status;
}

template <typename T>
static absl::Status GetStatus(const StatusOr<T>& status_or) {
  return status_or.status();
}

MATCHER(IsOK, "OK") {
  if (arg.ok()) return true;
  *result_listener << "status is " << GetStatus(arg);
  return false;
}

TEST(TempFile, Move) {
  // This test should typically be run under Address Sanitizer to debug issues
  // with the move constructor.
  absl::BitGen rnd;
  absl::optional<TempFile> outer;
  {
    auto status_or_file = TempFile::Create(TempDir(), "file-*.tmp", rnd);
    EXPECT_THAT(status_or_file, IsOK());
    auto& inner = status_or_file.value();
    EXPECT_THAT(inner.Write("h"), IsOK());
    outer = std::move(inner);
  }
  EXPECT_THAT(outer->Write("i"), IsOK());
  EXPECT_THAT(ReadFile(outer->path()), "hi");
  EXPECT_THAT(outer->Close(), IsOK());
}

TEST(TempFile, Create) {
  absl::BitGen rnd;
  auto status_or_file = TempFile::Create(TempDir(), "foo-*.tmp", rnd);
  ASSERT_THAT(status_or_file, IsOK());
  auto& file = status_or_file.value();
  const auto path = file.path();
  EXPECT_THAT(Parent(path), Eq(RemoveSlash(TempDir())));
  EXPECT_THAT(std::string(FileName(path)), StartsWith("foo-"));
  EXPECT_THAT(std::string(FileName(path)), EndsWith(".tmp"));
  EXPECT_THAT(FileExists(path), IsTrue());
  EXPECT_THAT(file.Close(), IsOK());
  EXPECT_THAT(file.path(), IsEmpty());
  EXPECT_THAT(FileExists(path), IsFalse());
}

TEST(TempFile, Assign) {
  absl::BitGen rnd;
  auto status_or_file = TempFile::Create(TempDir(), "foo-*.tmp", rnd);
  ASSERT_THAT(status_or_file, IsOK());
  auto a = std::move(status_or_file).value();
  auto b = std::move(a);
  std::swap(a, b);
  EXPECT_THAT(a.Close(), IsOK());
}

TEST(TempFile, Write) {
  absl::BitGen rnd;
  auto status_or_stream = TempFile::Create(TempDir(), "foo-*.tmp", rnd);
  ASSERT_THAT(status_or_stream, IsOK());
  auto& stream = status_or_stream.value();
  const auto path = stream.path();
  EXPECT_THAT(Parent(path), Eq(RemoveSlash(TempDir())));
  EXPECT_THAT(std::string(FileName(path)), StartsWith("foo-"));
  EXPECT_THAT(std::string(FileName(path)), EndsWith(".tmp"));
  EXPECT_THAT(FileExists(path), IsTrue());
  EXPECT_THAT(stream.Write("hello world\n"), IsOK());
  EXPECT_THAT(ReadFile(path), "hello world\n");
  EXPECT_THAT(stream.Close(), IsOK());
  EXPECT_THAT(stream.path(), IsEmpty());
  EXPECT_THAT(FileExists(path), IsFalse());
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

TEST(JoinPath, Root) {
  EXPECT_THAT(JoinPath("/", "file"), StrEq("/file"));
}

TEST(JoinPath, FinalSlash) {
  EXPECT_THAT(JoinPath("dir", "/"), StrEq("dir/"));
}

TEST(TempName, Create) {
  absl::BitGen rnd;
  const auto a = TempName("/", "temp-*.json", rnd);
  const auto b = TempName("/", "temp-*.json", rnd);
  EXPECT_THAT(a, StartsWith("/temp-"));
  EXPECT_THAT(a, EndsWith(".json"));
  EXPECT_THAT(b, StartsWith("/temp-"));
  EXPECT_THAT(b, EndsWith(".json"));
  EXPECT_THAT(b, StrNe(a));
}

}  // namespace
}  // namespace phst_rules_elisp
