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

#include "elisp/private/tools/runfiles.h"

#include <fstream>
#include <ios>
#include <optional>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/status/status_matchers.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/system.h"

namespace rules_elisp {

using absl_testing::IsOk;
using absl_testing::IsOkAndHolds;
using absl_testing::StatusIs;
using ::testing::IsEmpty;

TEST(RunfilesTest, ResolvesRunfile) {
  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(ExecutableKind::kTest, BAZEL_CURRENT_REPOSITORY, {});
  ASSERT_THAT(runfiles, IsOk());
  const absl::StatusOr<FileName> resolved =
      runfiles->Resolve(RULES_ELISP_PROGRAM);
  ASSERT_THAT(resolved, IsOk());
  const std::ifstream file(resolved->string(), std::ios::in | std::ios::binary);
  EXPECT_TRUE(file.is_open());
  EXPECT_TRUE(file.good());
}

TEST(RunfilesTest, RejectsNonAscii) {
  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(ExecutableKind::kTest, BAZEL_CURRENT_REPOSITORY, {});
  ASSERT_THAT(runfiles, IsOk());
  EXPECT_THAT(runfiles->Resolve("Foó"),
              StatusIs(absl::StatusCode::kInvalidArgument));
}

TEST(RunfilesTest, AllowsRunningTool) {
  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(ExecutableKind::kTest, BAZEL_CURRENT_REPOSITORY, {});
  ASSERT_THAT(runfiles, IsOk());
  const absl::StatusOr<FileName> program =
      runfiles->Resolve(RULES_ELISP_PROGRAM);
  ASSERT_THAT(program, IsOk());
  const absl::StatusOr<Environment> env = runfiles->Environ();
  ASSERT_THAT(env, IsOk());
  EXPECT_THAT(rules_elisp::Run(program->string(), {}, *env), IsOkAndHolds(0));
}

TEST(RunfilesTest, ParsesManifest) {
  absl::StatusOr<TemporaryFile> manifest = TemporaryFile::Create();
  ASSERT_THAT(manifest, IsOk());
  EXPECT_THAT(
      manifest->Write(kWindows ? "foo C:\\Bar\\Baz\n" : "foo /bar/baz\n"),
      IsOk());

  const FileName file =
      FileName::FromString(kWindows ? RULES_ELISP_NATIVE_LITERAL("C:\\Bar\\Baz")
                                    : RULES_ELISP_NATIVE_LITERAL("/bar/baz"))
          .value();

  const absl::StatusOr<Runfiles> runfiles = Runfiles::Create(
      BAZEL_CURRENT_REPOSITORY, {},
      FileName::FromString(manifest->name()).value(), std::nullopt);
  ASSERT_THAT(runfiles, IsOk());
  EXPECT_THAT(runfiles->Resolve("foo"), IsOkAndHolds(file));
  EXPECT_THAT(runfiles->Resolve("qux"), StatusIs(absl::StatusCode::kNotFound));
}

}  // namespace rules_elisp
