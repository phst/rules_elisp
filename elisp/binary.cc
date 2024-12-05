// Copyright 2020, 2021, 2022, 2023, 2024, 2025 Google LLC
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

#include "elisp/binary.h"

#include <initializer_list>

#include "absl/status/statusor.h"
#include "absl/types/span.h"

#include "elisp/platform.h"
#include "elisp/process.h"

namespace rules_elisp {

absl::StatusOr<int> Main(
    const std::initializer_list<NativeStringView> launcher_args,
    const absl::Span<const NativeStringView> original_args) {
  return RunLauncher(BAZEL_CURRENT_REPOSITORY, RULES_ELISP_RUN_BINARY,
                     {RULES_ELISP_BINARY_ARGS}, launcher_args, original_args,
                     ExecutableKind::kBinary);
}

}  // namespace rules_elisp
