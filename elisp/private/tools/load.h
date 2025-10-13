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

#ifndef ELISP_PRIVATE_TOOLS_LOAD_H_
#define ELISP_PRIVATE_TOOLS_LOAD_H_

#include <string_view>
#include <vector>

#include "absl/status/statusor.h"
#include "absl/types/span.h"
#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/runfiles.h"

namespace rules_elisp {

absl::StatusOr<std::vector<NativeString>> LoadPathArgs(
    const Runfiles& runfiles, absl::Span<const NativeStringView> load_path,
    const std::string_view runfiles_elc);

}  // namespace

#endif  // ELISP_PRIVATE_TOOLS_LOAD_H_
