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

#include "elisp/private/tools/emacs.h"

#include <cstddef>
#include <iterator>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/log/check.h"
#include "absl/status/status.h"
#include "absl/status/status_macros.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/types/span.h"

#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/runfiles.h"
#include "elisp/private/tools/strings.h"
#include "elisp/private/tools/system.h"

namespace rules_elisp {

static absl::StatusOr<int> RunEmacs(
    const std::string_view source_repository, const RepositoryType type,
    const std::string_view install,
    const absl::Span<const NativeStringView> original_args) {
  ABSL_ASSIGN_OR_RETURN(const Runfiles runfiles,
                        Runfiles::Create(ExecutableKind::kBinary,
                                         source_repository, original_args));
  bool release;
  // We currently support pre-built Emacsen only on Windows because there are no
  // official binary release archives for Unix systems.
  if (kWindows && type == RepositoryType::kRelease) {
    release = true;
  } else {
    CHECK_EQ(type, RepositoryType::kSource) << "invalid repository type";
    release = false;
  }
  std::optional<FileName> emacs;
  std::optional<DosDevice> dos_device;
  if (kWindows && release) {
    ABSL_ASSIGN_OR_RETURN(const FileName root, runfiles.Resolve(install));
    // The filenames in the released Emacs archive are too long.  Create a
    // drive letter to shorten them.
    ABSL_ASSIGN_OR_RETURN(DosDevice device, DosDevice::Create(root));
    ABSL_ASSIGN_OR_RETURN(
        FileName program,
        FileName::FromString(device.name() +
                             RULES_ELISP_NATIVE_LITERAL("\\bin\\emacs.exe")));
    emacs = std::move(program);
    dos_device = std::move(device);
  } else {
    ABSL_ASSIGN_OR_RETURN(
        FileName binary,
        runfiles.Resolve(
            absl::StrCat(install, release ? "/bin/emacs.exe" : "/emacs.exe")));
    emacs = std::move(binary);
  }
  CHECK(emacs.has_value());
  std::vector<NativeString> args;
  if (!release) {
    ABSL_ASSIGN_OR_RETURN(const FileName dump, runfiles.Resolve(absl::StrCat(
                                                   install, "/emacs.pdmp")));
    args.push_back(RULES_ELISP_NATIVE_LITERAL("--dump-file=") + dump.string());
  }
  if (!original_args.empty()) {
    args.insert(args.end(), std::next(original_args.begin()),
                original_args.end());
  }
  ABSL_ASSIGN_OR_RETURN(Environment env, runfiles.Environ());
  if (!release) {
    ABSL_ASSIGN_OR_RETURN(const FileName etc,
                          runfiles.Resolve(absl::StrCat(install, "/etc")));
    ABSL_ASSIGN_OR_RETURN(const FileName lisp,
                          runfiles.Resolve(absl::StrCat(install, "/lisp")));
    ABSL_ASSIGN_OR_RETURN(const FileName libexec,
                          runfiles.Resolve(absl::StrCat(install, "/libexec")));
    env.Add(RULES_ELISP_NATIVE_LITERAL("EMACSDATA"), etc.string());
    env.Add(RULES_ELISP_NATIVE_LITERAL("EMACSDOC"), etc.string());
    env.Add(RULES_ELISP_NATIVE_LITERAL("EMACSLOADPATH"), lisp.string());
    env.Add(RULES_ELISP_NATIVE_LITERAL("EMACSPATH"), libexec.string());
  }
  ABSL_ASSIGN_OR_RETURN(Environment orig_env, Environment::Current());
  env.Merge(orig_env);
  if constexpr (kWindows) {
    // On Windows, Emacs doesn’t support Unicode arguments or environment
    // variables.  Check here rather than sending over garbage.
    for (const NativeString& arg : args) {
      ABSL_RETURN_IF_ERROR(CheckAscii(arg));
    }
    for (const auto& [name, value] : env) {
      ABSL_RETURN_IF_ERROR(CheckAscii(name));
      ABSL_RETURN_IF_ERROR(CheckAscii(value));
    }
  }
  return RunProcess(*emacs, args, env);
}

absl::StatusOr<int> Main(
    const RepositoryType type, const std::string_view install,
    const absl::Span<const NativeStringView> original_args) {
  return RunEmacs(BAZEL_CURRENT_REPOSITORY, type, install, original_args);
}

}  // namespace rules_elisp
