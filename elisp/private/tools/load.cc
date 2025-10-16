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

#include "elisp/private/tools/load.h"

#include <string>
#include <string_view>
#include <vector>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_format.h"
#include "absl/types/span.h"
#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/runfiles.h"
#include "elisp/private/tools/strings.h"

namespace rules_elisp {

absl::StatusOr<std::vector<NativeString>> LoadPathArgs(
    const Runfiles& runfiles, const absl::Span<const NativeString> load_path,
    const std::string_view runfiles_elc) {
  std::vector<NativeString> args;
  bool runfiles_handler_installed = false;
  for (const NativeStringView dir : load_path) {
    if (dir.empty()) return absl::InvalidArgumentError("Empty load directory");
    if (ContainsNull(dir)) {
      return absl::InvalidArgumentError(
          absl::StrFormat("Load directory %s contains null character", dir));
    }
    const absl::StatusOr<std::string> narrow = ToNarrow(dir, Encoding::kAscii);
    const absl::StatusOr<NativeString> resolved =
        narrow.ok() ? runfiles.Resolve(*narrow) : narrow.status();
    if (resolved.ok()) {
      args.push_back(RULES_ELISP_NATIVE_LITERAL("--directory=") + *resolved);
    } else {
      if (!runfiles_handler_installed) {
        const absl::StatusOr<NativeString> file =
            runfiles.Resolve(runfiles_elc);
        if (!file.ok()) return file.status();
        args.push_back(RULES_ELISP_NATIVE_LITERAL("--load=") + *file);
        args.push_back(RULES_ELISP_NATIVE_LITERAL(
            "--funcall=elisp/runfiles/install-handler"));
        runfiles_handler_installed = true;
      }
      args.push_back(RULES_ELISP_NATIVE_LITERAL("--directory=/bazel-runfile:") +
                     static_cast<NativeString>(dir));
    }
  }
  return args;
}

}  // namespace rules_elisp
