// Copyright 2020-2025 Google LLC
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

#include "elisp/private/tools/test.h"

#include <vector>

#include "absl/status/statusor.h"
#include "absl/types/span.h"

#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/process.h"
#include "elisp/private/tools/runfiles.h"

namespace rules_elisp {

absl::StatusOr<int> Main(
    const CommonOptions& common_opts, const TestOptions& test_opts,
    const absl::Span<const NativeStringView> original_args) {
  std::vector<NativeString> launcher_args;
  for (const NativeStringView test : test_opts.skip_tests) {
    launcher_args.push_back(RULES_ELISP_NATIVE_LITERAL("--skip-test=") +
                            static_cast<NativeString>(test));
  }
  for (const NativeStringView tag : test_opts.skip_tags) {
    launcher_args.push_back(RULES_ELISP_NATIVE_LITERAL("--skip-tag=") +
                            static_cast<NativeString>(tag));
  }
  if (test_opts.module_assertions) {
    launcher_args.push_back(RULES_ELISP_NATIVE_LITERAL("--module-assertions"));
  }
  return RunLauncher(BAZEL_CURRENT_REPOSITORY, RULES_ELISP_RUN_TEST,
                     common_opts, {}, launcher_args, original_args,
                     ExecutableKind::kTest);
}

}  // namespace rules_elisp
