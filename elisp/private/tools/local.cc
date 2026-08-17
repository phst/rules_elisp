// Copyright 2020-2026 Google LLC
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

#include "elisp/private/tools/local.h"

#include <cstddef>
#include <iterator>
#include <vector>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/types/span.h"

#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/strings.h"
#include "elisp/private/tools/system.h"

namespace rules_elisp {

static absl::StatusOr<int> RunEmacs(
    const NativeStringView program,
    const absl::Span<const NativeStringView> original_args) {
  if (program.empty()) return absl::NotFoundError("Emacs program not found");
  const absl::StatusOr<FileName> emacs = FileName::FromString(program);
  if (!emacs.ok()) return emacs.status();
  std::vector<NativeString> args;
  if (!original_args.empty()) {
    args.insert(args.end(), std::next(original_args.begin()),
                original_args.end());
  }
  absl::StatusOr<Environment> env = Environment::Current();
  if (!env.ok()) return env.status();
  if constexpr (kWindows) {
    // On Windows, Emacs doesn’t support Unicode arguments or environment
    // variables.  Check here rather than sending over garbage.
    for (const NativeString& arg : args) {
      if (const absl::Status status = CheckAscii(arg); !status.ok()) {
        return status;
      }
    }
    for (const auto& [name, value] : *env) {
      if (const absl::Status status = CheckAscii(name); !status.ok()) {
        return status;
      }
      if (const absl::Status status = CheckAscii(value); !status.ok()) {
        return status;
      }
    }
  }
  return RunProcess(*emacs, args, *env);
}

absl::StatusOr<int> Main(const NativeStringView program,
                         const absl::Span<const NativeStringView> args) {
  return RunEmacs(program, args);
}

}  // namespace rules_elisp
