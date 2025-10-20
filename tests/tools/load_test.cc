// Copyright 2021-2025 Google LLC
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

#include "elisp/private/tools/load.h"

#include <cstddef>
#include <string>

#include "absl/algorithm/container.h"
#include "absl/status/status.h"
#include "absl/status/status_matchers.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_format.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/runfiles.h"
#include "elisp/private/tools/system.h"

namespace rules_elisp {

using absl_testing::IsOk;
using absl_testing::IsOkAndHolds;
using ::testing::ElementsAre;

static absl::StatusOr<NativeString> TempDir() {
  std::string dir = ::testing::TempDir();
  if (dir.empty()) {
    return absl::FailedPreconditionError("Empty temporary directory");
  }
  if (dir.back() == '/' || (kWindows && dir.back() == '\\')) dir.pop_back();
  if constexpr (kWindows) absl::c_replace(dir, '/', '\\');
  return ToNative(dir, Encoding::kAscii);
}

TEST(LoadPathArgsTest, DirectoryAsciiOnly) {
  const absl::StatusOr<NativeString> dir = TempDir();
  ASSERT_THAT(dir, IsOk());

  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(BAZEL_CURRENT_REPOSITORY, {}, {}, *dir);
  ASSERT_THAT(runfiles, IsOk());

  EXPECT_THAT(LoadPathArgs(*runfiles,
                           {
                               RULES_ELISP_NATIVE_LITERAL("foo"),
                               RULES_ELISP_NATIVE_LITERAL("bar '"),
                           },
                           "unused/runfiles.elc"),
              IsOkAndHolds(ElementsAre(
                  RULES_ELISP_NATIVE_LITERAL("--directory=") + *dir +
                      kSeparator + RULES_ELISP_NATIVE_LITERAL("foo"),
                  RULES_ELISP_NATIVE_LITERAL("--directory=") + *dir +
                      kSeparator + RULES_ELISP_NATIVE_LITERAL("bar '"))));
}

TEST(LoadPathArgsTest, DirectoryNonAscii) {
  const absl::StatusOr<NativeString> dir = TempDir();
  ASSERT_THAT(dir, IsOk());

  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(BAZEL_CURRENT_REPOSITORY, {}, {}, *dir);
  ASSERT_THAT(runfiles, IsOk());

  EXPECT_THAT(LoadPathArgs(*runfiles,
                           {
                               RULES_ELISP_NATIVE_LITERAL("foo"),
                               RULES_ELISP_NATIVE_LITERAL("bar äα𝐴🐈'"),
                           },
                           "runfiles.elc"),
              IsOkAndHolds(ElementsAre(
                  RULES_ELISP_NATIVE_LITERAL("--directory=") + *dir +
                      kSeparator + RULES_ELISP_NATIVE_LITERAL("foo"),
                  RULES_ELISP_NATIVE_LITERAL("--load=") + *dir + kSeparator +
                      RULES_ELISP_NATIVE_LITERAL("runfiles.elc"),
                  RULES_ELISP_NATIVE_LITERAL(
                      "--funcall=elisp/runfiles/install-handler"),
                  RULES_ELISP_NATIVE_LITERAL(
                      "--directory=/bazel-runfile:bar äα𝐴🐈'"))));
}

TEST(LoadPathArgsTest, Manifest) {
  const NativeString runfiles_dir =
      kWindows ? RULES_ELISP_NATIVE_LITERAL("C:\\Runfiles")
               : RULES_ELISP_NATIVE_LITERAL("/runfiles");
  const NativeString runfiles_elc =
      runfiles_dir + kSeparator + RULES_ELISP_NATIVE_LITERAL("runfiles.elc");

  absl::StatusOr<TemporaryFile> file = TemporaryFile::Create();
  ASSERT_THAT(file, IsOk());
  std::string line =
      absl::StrFormat("repository/runfiles.elc %s\n", runfiles_elc);
  // Runfiles manifests contain POSIX-style filenames even on Windows.
  if constexpr (kWindows) absl::c_replace(line, '\\', '/');
  EXPECT_THAT(file->Write(line), IsOk());

  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(BAZEL_CURRENT_REPOSITORY, {}, file->name(), {});
  ASSERT_THAT(runfiles, IsOk());

  EXPECT_THAT(LoadPathArgs(*runfiles,
                           {
                               RULES_ELISP_NATIVE_LITERAL("foo"),
                               RULES_ELISP_NATIVE_LITERAL("bar äα𝐴🐈'"),
                           },
                           "repository/runfiles.elc"),
              IsOkAndHolds(ElementsAre(
                  RULES_ELISP_NATIVE_LITERAL("--load=") + runfiles_elc,
                  RULES_ELISP_NATIVE_LITERAL(
                      "--funcall=elisp/runfiles/install-handler"),
                  RULES_ELISP_NATIVE_LITERAL("--directory=/bazel-runfile:foo"),
                  RULES_ELISP_NATIVE_LITERAL(
                      "--directory=/bazel-runfile:bar äα𝐴🐈'"))));
}

}  // namespace rules_elisp
