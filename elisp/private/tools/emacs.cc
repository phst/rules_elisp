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

#include "elisp/private/tools/emacs.h"

#include <string_view>

#include "absl/status/statusor.h"
#include "absl/types/span.h"

#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/process.h"

namespace rules_elisp {

absl::StatusOr<int> Main(
    const Params params,
    const absl::Span<const NativeStringView> original_args) {
  return RunEmacs(BAZEL_CURRENT_REPOSITORY, params.mode, params.install,
                  original_args);
}

}  // namespace rules_elisp
