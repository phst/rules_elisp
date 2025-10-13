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

#ifndef ELISP_PROCESS_H_
#define ELISP_PROCESS_H_

#if !defined __cplusplus || __cplusplus < 201703L
#  error this file requires at least C++17
#endif

#include <initializer_list>
#include <string_view>

#include "absl/status/statusor.h"
#include "absl/types/span.h"

#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/runfiles.h"

namespace rules_elisp {

absl::StatusOr<int> RunLauncher(
    std::string_view source_repository, std::string_view binary,
    const CommonOptions& common_opts,
    std::initializer_list<NativeStringView> common_args,
    absl::Span<const NativeString> launcher_args,
    absl::Span<const NativeStringView> original_args, ExecutableKind kind);

absl::StatusOr<int> RunEmacs(std::string_view source_repository, Mode mode,
                             std::string_view install,
                             absl::Span<const NativeStringView> original_args);

}  // namespace rules_elisp

#endif  // ELISP_PROCESS_H_
