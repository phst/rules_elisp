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

#include "elisp/private/tools/binary.h"

#include <string>

#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/types/span.h"

#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/process.h"

namespace rules_elisp {

#ifdef _WIN32
static std::wstring ToNativeString(const int i) {
  return std::to_wstring(i);
}
#else
static std::string ToNativeString(const int i) {
  return absl::StrCat(i);
}
#endif

absl::StatusOr<int> Main(
    const CommonOptions& common_opts, const BinaryOptions& binary_opts,
    const absl::Span<const NativeStringView> original_args) {
  std::vector<NativeString> launcher_args;
  if (binary_opts.interactive) {
    launcher_args.push_back(RULES_ELISP_NATIVE_LITERAL("--interactive"));
  }
  for (const int i : binary_opts.input_args) {
    launcher_args.push_back(RULES_ELISP_NATIVE_LITERAL("--input-arg=") +
                            ToNativeString(i));
  }
  for (const int i : binary_opts.output_args) {
    launcher_args.push_back(RULES_ELISP_NATIVE_LITERAL("--output-arg=") +
                            ToNativeString(i));
  }
  return RunLauncher(BAZEL_CURRENT_REPOSITORY, RULES_ELISP_RUN_BINARY,
                     common_opts, {RULES_ELISP_BINARY_ARGS}, launcher_args,
                     original_args, ExecutableKind::kBinary);
}

}  // namespace rules_elisp
