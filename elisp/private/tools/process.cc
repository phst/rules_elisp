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

#include "elisp/private/tools/process.h"

#include <cstdlib>
#include <initializer_list>
#include <initializer_list>
#include <string>
#include <string_view>
#include <vector>

#include "absl/log/check.h"
#include "absl/status/statusor.h"
#include "absl/types/span.h"

#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/runfiles.h"
#include "elisp/private/tools/system.h"

namespace rules_elisp {

absl::StatusOr<int> RunLauncher(
    const std::string_view source_repository, const std::string_view binary,
    const CommonOptions& common_opts,
    const std::initializer_list<NativeStringView> common_args,
    const absl::Span<const NativeString> launcher_args,
    const absl::Span<const NativeStringView> original_args,
    const ExecutableKind kind) {
  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(kind, source_repository, original_args);
  if (!runfiles.ok()) return runfiles.status();
  absl::StatusOr<NativeString> resolved_binary = runfiles->Resolve(binary);
  if (!resolved_binary.ok()) return resolved_binary.status();
  std::vector<NativeString> final_args{*resolved_binary};
  final_args.insert(final_args.end(), common_args.begin(), common_args.end());
  final_args.push_back(RULES_ELISP_NATIVE_LITERAL("--wrapper=") +
                       static_cast<NativeString>(common_opts.wrapper));
  final_args.push_back(common_opts.mode == rules_elisp::ToolchainMode::kWrap
                           ? RULES_ELISP_NATIVE_LITERAL("--mode=wrap")
                           : RULES_ELISP_NATIVE_LITERAL("--mode=direct"));
  for (const NativeStringView tag : common_opts.tags) {
    final_args.push_back(RULES_ELISP_NATIVE_LITERAL("--rule-tag=") +
                         static_cast<NativeString>(tag));
  }
  for (const NativeStringView dir : common_opts.load_path) {
    final_args.push_back(RULES_ELISP_NATIVE_LITERAL("--load-directory=") +
                         static_cast<NativeString>(dir));
  }
  for (const NativeStringView file : common_opts.load_files) {
    final_args.push_back(RULES_ELISP_NATIVE_LITERAL("--load-file=") +
                         static_cast<NativeString>(file));
  }
  for (const NativeStringView file : common_opts.data_files) {
    final_args.push_back(RULES_ELISP_NATIVE_LITERAL("--data-file=") +
                         static_cast<NativeString>(file));
  }
  final_args.insert(final_args.end(), launcher_args.begin(),
                    launcher_args.end());
  final_args.push_back(RULES_ELISP_NATIVE_LITERAL("--"));
  final_args.insert(final_args.end(), original_args.begin(),
                    original_args.end());
  absl::StatusOr<Environment> merged_env = runfiles->Environ();
  if (!merged_env.ok()) return merged_env.status();
  absl::StatusOr<Environment> orig_env = Environment::Current();
  if (!orig_env.ok()) return orig_env.status();
  // We don’t want the Python launcher to change the current working directory,
  // otherwise relative filenames will be all messed up.  See
  // https://github.com/bazelbuild/bazel/issues/7190.
  orig_env->Remove(RULES_ELISP_NATIVE_LITERAL("RUN_UNDER_RUNFILES"));
  merged_env->Merge(*orig_env);
  return Run(final_args, *merged_env);
}

}  // namespace rules_elisp
