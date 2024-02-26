// Copyright 2020, 2021, 2022, 2023, 2024 Google LLC
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

#include "elisp/main.h"

#include <initializer_list>
#include <string_view>
#include <vector>

#ifdef __GNUC__
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wpedantic"
#  pragma GCC diagnostic ignored "-Wconversion"
#  pragma GCC diagnostic ignored "-Wsign-conversion"
#  pragma GCC diagnostic ignored "-Woverflow"
#endif
#ifdef _MSC_VER
#  pragma warning(push, 3)
#endif
#include "absl/status/statusor.h"
#include "absl/types/span.h"
#ifdef __GNUC__
#  pragma GCC diagnostic pop
#endif
#ifdef _MSC_VER
#  pragma warning(pop)
#endif

#include "elisp/platform.h"
#include "elisp/process.h"

namespace rules_elisp {

absl::StatusOr<int> Main(
    const std::initializer_list<NativeStringView> launcher_args,
    const absl::Span<const NativeStringView> original_args) {
  std::vector<NativeStringView> args = {RULES_ELISP_BINARY_ARGS};
  args.insert(args.end(), launcher_args.begin(), launcher_args.end());
  args.push_back(RULES_ELISP_NATIVE_LITERAL("--"));
  args.insert(args.end(), original_args.begin(), original_args.end());
  return Run(
      BAZEL_CURRENT_REPOSITORY, RULES_ELISP_RUN_BINARY, args,
      ExecutableKind::kBinary,
      original_args.empty() ? NativeStringView() : original_args.front());
}

}  // namespace rules_elisp
