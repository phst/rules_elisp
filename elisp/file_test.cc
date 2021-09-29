// Copyright 2020, 2021 Google LLC
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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#pragma GCC diagnostic pop

namespace phst_rules_elisp {
namespace {

using ::testing::StrEq;

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

}  // namespace
}  // namespace phst_rules_elisp
