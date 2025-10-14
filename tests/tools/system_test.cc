// Copyright 2025 Philipp Stephani
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include "elisp/private/tools/system.h"

#include <stdlib.h>

#include <cerrno>
#include <cstdlib>
#include <fstream>
#include <ios>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "absl/base/nullability.h"
#include "absl/status/status.h"
#include "absl/status/status_matchers.h"
#include "absl/status/statusor.h"
#include "absl/strings/ascii.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

#include "elisp/private/tools/numeric.h"
#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/runfiles.h"
#include "elisp/private/tools/strings.h"

namespace rules_elisp {

using ::testing::Contains;
using ::testing::EndsWith;
using ::testing::Ge;
using ::testing::Gt;
using ::testing::Pair;
using ::testing::SizeIs;
using ::testing::StartsWith;
using absl_testing::IsOk;
using absl_testing::IsOkAndHolds;
using absl_testing::StatusIs;

TEST(ErrnoStatusTest, ReturnsMatchingStatus) {
  errno = ENOENT;
  EXPECT_THAT(ErrnoStatus("foo"), StatusIs(absl::StatusCode::kNotFound));
}

TEST(IsAbsoluteTest, Works) {
  EXPECT_FALSE(IsAbsolute(RULES_ELISP_NATIVE_LITERAL("")));
  EXPECT_FALSE(IsAbsolute(RULES_ELISP_NATIVE_LITERAL(".")));
  EXPECT_FALSE(IsAbsolute(RULES_ELISP_NATIVE_LITERAL("foo")));
  EXPECT_FALSE(IsAbsolute(RULES_ELISP_NATIVE_LITERAL("foo/bar")));
  if constexpr (kWindows) {
    // See
    // https://googleprojectzero.blogspot.com/2016/02/the-definitive-guide-on-win32-to-nt.html
    // for the various kinds of filenames on Windows.
    EXPECT_TRUE(IsAbsolute(RULES_ELISP_NATIVE_LITERAL("C:\\")));
    EXPECT_TRUE(IsAbsolute(RULES_ELISP_NATIVE_LITERAL("C:/")));
    EXPECT_TRUE(IsAbsolute(RULES_ELISP_NATIVE_LITERAL("C:\\Foo")));
    EXPECT_TRUE(IsAbsolute(RULES_ELISP_NATIVE_LITERAL("C:/Foo")));
    EXPECT_FALSE(IsAbsolute(RULES_ELISP_NATIVE_LITERAL("C:")));
    EXPECT_FALSE(IsAbsolute(RULES_ELISP_NATIVE_LITERAL("C:Foo")));
    EXPECT_FALSE(IsAbsolute(RULES_ELISP_NATIVE_LITERAL("\\Foo")));
    EXPECT_FALSE(IsAbsolute(RULES_ELISP_NATIVE_LITERAL("/Foo")));
    EXPECT_FALSE(IsAbsolute(RULES_ELISP_NATIVE_LITERAL("NUL")));
  } else {
    EXPECT_TRUE(IsAbsolute(RULES_ELISP_NATIVE_LITERAL("/")));
    EXPECT_TRUE(IsAbsolute(RULES_ELISP_NATIVE_LITERAL("/foo")));
  }
}

TEST(MakeAbsoluteTest, RejectsEmptyName) {
  EXPECT_THAT(MakeAbsolute(RULES_ELISP_NATIVE_LITERAL("")),
              StatusIs(absl::StatusCode::kInvalidArgument));
}

TEST(MakeAbsoluteTest, KeepsAbsoluteName) {
  constexpr NativeStringView file =
      kWindows ? RULES_ELISP_NATIVE_LITERAL("C:\\Foo\\Bar.txt")
               : RULES_ELISP_NATIVE_LITERAL("/foo/bar.txt");
  EXPECT_THAT(MakeAbsolute(file), IsOkAndHolds(file));
}

TEST(MakeAbsoluteTest, MakesRelativeNameAbsolute) {
  const absl::StatusOr<NativeString> file =
      MakeAbsolute(RULES_ELISP_NATIVE_LITERAL("foo.txt"));
  ASSERT_THAT(file, IsOk());
  if constexpr (kWindows) {
    ASSERT_THAT(*file, SizeIs(Gt(3)));
    const std::optional<unsigned char> drive =
        CastNumber<unsigned char>(file->at(0));
    ASSERT_NE(drive, std::nullopt);
    EXPECT_TRUE(absl::ascii_isalpha(*drive));
    EXPECT_EQ(file->at(1), RULES_ELISP_NATIVE_LITERAL(':'));
    EXPECT_EQ(file->at(2), RULES_ELISP_NATIVE_LITERAL('\\'));
    EXPECT_THAT(*file, EndsWith(RULES_ELISP_NATIVE_LITERAL("\\foo.txt")));
  } else {
    EXPECT_THAT(*file, StartsWith(RULES_ELISP_NATIVE_LITERAL("/")));
    EXPECT_THAT(*file, EndsWith(RULES_ELISP_NATIVE_LITERAL("/foo.txt")));
  }
}

TEST(EnvironmentTest, CurrentReturnsValidEnv) {
  const absl::StatusOr<Environment> env = Environment::Current();
  ASSERT_THAT(
      env, IsOkAndHolds(Contains(Pair(RULES_ELISP_NATIVE_LITERAL("TEST_SRCDIR"),
                                      SizeIs(Gt(0))))));
}

TEST(EnvironmentTest, CreateRejectsEmpty) {
  const std::vector<std::pair<NativeString, NativeString>> pairs = {
      {RULES_ELISP_NATIVE_LITERAL("foo"), RULES_ELISP_NATIVE_LITERAL("bar")},
      {RULES_ELISP_NATIVE_LITERAL(""), RULES_ELISP_NATIVE_LITERAL("baz")},
  };
  EXPECT_THAT(Environment::Create(pairs.cbegin(), pairs.cend()),
              StatusIs(absl::StatusCode::kInvalidArgument));
}

TEST(EnvironmentTest, CreateRejectsDuplicates) {
  std::vector<std::pair<NativeString, NativeString>> pairs = {
      {RULES_ELISP_NATIVE_LITERAL("foo"), RULES_ELISP_NATIVE_LITERAL("bar")},
      {RULES_ELISP_NATIVE_LITERAL("foo"), RULES_ELISP_NATIVE_LITERAL("baz")},
  };
  EXPECT_THAT(Environment::Create(pairs.cbegin(), pairs.cend()),
              StatusIs(absl::StatusCode::kAlreadyExists));
  if constexpr (kWindows) {
    pairs.emplace_back(RULES_ELISP_NATIVE_LITERAL("Foo"),
                       RULES_ELISP_NATIVE_LITERAL("Bar"));
    EXPECT_THAT(Environment::Create(pairs.cbegin(), pairs.cend()),
              StatusIs(absl::StatusCode::kAlreadyExists));
  }
}

TEST(EnvironmentTest, CreateIsCaseSensitiveOnlyOnUnix) {
  const std::vector<std::pair<NativeString, NativeString>> pairs = {
      {RULES_ELISP_NATIVE_LITERAL("foo"), RULES_ELISP_NATIVE_LITERAL("bar")},
      {RULES_ELISP_NATIVE_LITERAL("Foo"), RULES_ELISP_NATIVE_LITERAL("baz")},
  };
  const absl::StatusOr<Environment> env =
      Environment::Create(pairs.cbegin(), pairs.cend());
  if constexpr (kWindows) {
    EXPECT_THAT(env, StatusIs(absl::StatusCode::kAlreadyExists));
  } else {
    EXPECT_THAT(env, IsOkAndHolds(SizeIs(pairs.size())));
  }
}

TEST(EnvironmentTest, AddIsCaseSensitiveOnlyOnUnix) {
  const std::vector<std::pair<NativeString, NativeString>> pairs = {
      {RULES_ELISP_NATIVE_LITERAL("foo"), RULES_ELISP_NATIVE_LITERAL("bar")},
  };
  absl::StatusOr<Environment> env =
      Environment::Create(pairs.cbegin(), pairs.cend());
  ASSERT_THAT(env, IsOkAndHolds(SizeIs(pairs.size())));
  env->Add(RULES_ELISP_NATIVE_LITERAL("Foo"),
           RULES_ELISP_NATIVE_LITERAL("baz"));
  EXPECT_THAT(*env, SizeIs(pairs.size() + (kWindows ? 0 : 1)));
}

TEST(EnvironmentTest, RemoveIsCaseSensitiveOnlyOnUnix) {
  const std::vector<std::pair<NativeString, NativeString>> pairs = {
      {RULES_ELISP_NATIVE_LITERAL("foo"), RULES_ELISP_NATIVE_LITERAL("bar")},
  };
  absl::StatusOr<Environment> env =
      Environment::Create(pairs.cbegin(), pairs.cend());
  ASSERT_THAT(env, IsOkAndHolds(SizeIs(pairs.size())));
  env->Remove(RULES_ELISP_NATIVE_LITERAL("Foo"));
  EXPECT_THAT(*env, SizeIs(pairs.size() - (kWindows ? 1 : 0)));
}

[[maybe_unused]] static NativeString GetEnv(const NativeChar* const name) {
  const NativeChar* const absl_nullable value =
#ifdef _WIN32
      _wgetenv(name)
#else
      std::getenv(name)
#endif
      ;
  return value == nullptr ? NativeString() : NativeString(value);
}

TEST(RunTest, ReturnsExitCode) {
  if constexpr (kWindows) {
    const NativeString cmd = GetEnv(RULES_ELISP_NATIVE_LITERAL("ComSpec"));
    EXPECT_THAT(cmd, SizeIs(Gt(3)));
    EXPECT_THAT(rules_elisp::Run(
                    {
                        cmd,
                        RULES_ELISP_NATIVE_LITERAL("/U"),
                        RULES_ELISP_NATIVE_LITERAL("/D"),
                        RULES_ELISP_NATIVE_LITERAL("/S"),
                        RULES_ELISP_NATIVE_LITERAL("/C"),
                        RULES_ELISP_NATIVE_LITERAL("EXIT 0"),
                    },
                    {}),
                IsOkAndHolds(0));
    EXPECT_THAT(rules_elisp::Run(
                    {
                        cmd,
                        RULES_ELISP_NATIVE_LITERAL("/U"),
                        RULES_ELISP_NATIVE_LITERAL("/D"),
                        RULES_ELISP_NATIVE_LITERAL("/S"),
                        RULES_ELISP_NATIVE_LITERAL("/C"),
                        RULES_ELISP_NATIVE_LITERAL("EXIT 23"),
                    },
                    {}),
                IsOkAndHolds(23));
  } else {
    EXPECT_THAT(
        rules_elisp::Run({RULES_ELISP_NATIVE_LITERAL("/usr/bin/true")}, {}),
        IsOkAndHolds(0));
    EXPECT_THAT(
        rules_elisp::Run({RULES_ELISP_NATIVE_LITERAL("/usr/bin/false")}, {}),
        IsOkAndHolds(1));
  }
}

TEST(DosDeviceTest, CreatesDevice) {
  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(ExecutableKind::kTest, BAZEL_CURRENT_REPOSITORY, {});
  ASSERT_THAT(runfiles, IsOk());
  const absl::StatusOr<NativeString> file = runfiles->Resolve(RULES_ELISP_DATA);
  ASSERT_THAT(file, IsOk());
  const NativeString::size_type i =
      file->rfind(kWindows ? RULES_ELISP_NATIVE_LITERAL('\\')
                           : RULES_ELISP_NATIVE_LITERAL('/'));
  ASSERT_NE(i, file->npos);
  const NativeString dir = file->substr(0, i + 1);
  absl::StatusOr<DosDevice> dev = DosDevice::Create(dir);
  if constexpr (kWindows) {
    EXPECT_THAT(dir, SizeIs(Ge(3)));
    ASSERT_THAT(dev, IsOk());
    const NativeString name = dev->name();
    ASSERT_THAT(name, SizeIs(2));
    ASSERT_THAT(CheckASCII(name), IsOk());
    const std::optional<unsigned char> drive =
        CastNumber<unsigned char>(name.at(0));
    ASSERT_NE(drive, std::nullopt);
    EXPECT_TRUE(absl::ascii_isalpha(*drive));
    EXPECT_EQ(name.at(1), RULES_ELISP_NATIVE_LITERAL(':'));
    const std::ifstream stream(name + file->substr(i),
                               std::ios::in | std::ios::binary);
    EXPECT_TRUE(stream.is_open());
    EXPECT_TRUE(stream.good());
  }
}

}  // namespace rules_elisp
