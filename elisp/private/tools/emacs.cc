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

#include <cstddef>
#include <iterator>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/log/check.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/types/span.h"

#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/runfiles.h"
#include "elisp/private/tools/strings.h"
#include "elisp/private/tools/system.h"

namespace rules_elisp {

static absl::StatusOr<int> RunEmacs(
    const std::string_view source_repository, const Mode mode,
    const std::string_view install,
    const absl::Span<const NativeStringView> original_args) {
  const absl::StatusOr<Runfiles> runfiles = Runfiles::Create(
      ExecutableKind::kBinary, source_repository, original_args);
  if (!runfiles.ok()) return runfiles.status();
  bool release;
  // We currently support pre-built Emacsen only on Windows because there are no
  // official binary release archives for Unix systems.
  if (kWindows && mode == Mode::kRelease) {
    release = true;
  } else {
    CHECK_EQ(mode, Mode::kSource) << "invalid mode";
    release = false;
  }
  NativeString emacs;
  std::optional<DosDevice> dos_device;
  if (kWindows && release) {
    const absl::StatusOr<NativeString> root = runfiles->Resolve(install);
    if (!root.ok()) return root.status();
    // The longest filename in the Emacs release archive has 140 characters.
    // Round up to 150 for some buffer and the directory separator.
    constexpr std::size_t kMaxEntry = 150;
    const std::size_t max_filename = MaxFilename();
    CHECK_GT(max_filename, kMaxEntry);
    const std::size_t max_root = max_filename - kMaxEntry;
    if (root->length() > max_root) {
      // The filenames in the released Emacs archive are too long.  Create a
      // drive letter to shorten them.
      absl::StatusOr<DosDevice> device = DosDevice::Create(*root);
      if (!device.ok()) return device.status();
      emacs = device->name() + RULES_ELISP_NATIVE_LITERAL("\\bin\\emacs.exe");
      dos_device = std::move(*device);
    }
  }
  if (!dos_device.has_value()) {
    const absl::StatusOr<NativeString> binary = runfiles->Resolve(
        absl::StrCat(install, release ? "/bin/emacs.exe" : "/emacs.exe"));
    if (!binary.ok()) return binary.status();
    emacs = *binary;
  }
  CHECK(!emacs.empty());
  std::vector<NativeString> args = {emacs};
  if (!release) {
    const absl::StatusOr<NativeString> dump =
        runfiles->Resolve(absl::StrCat(install, "/emacs.pdmp"));
    if (!dump.ok()) return dump.status();
    args.push_back(RULES_ELISP_NATIVE_LITERAL("--dump-file=") + *dump);
  }
  if (!original_args.empty()) {
    args.insert(args.end(), std::next(original_args.begin()),
                original_args.end());
  }
  absl::StatusOr<Environment> env = runfiles->Environ();
  if (!env.ok()) return env.status();
  if (!release) {
    const absl::StatusOr<NativeString> etc =
        runfiles->Resolve(absl::StrCat(install, "/etc"));
    if (!etc.ok()) return etc.status();
    const absl::StatusOr<NativeString> lisp =
        runfiles->Resolve(absl::StrCat(install, "/lisp"));
    if (!lisp.ok()) return lisp.status();
    const absl::StatusOr<NativeString> libexec =
        runfiles->Resolve(absl::StrCat(install, "/libexec"));
    if (!libexec.ok()) return libexec.status();
    env->Add(RULES_ELISP_NATIVE_LITERAL("EMACSDATA"), *etc);
    env->Add(RULES_ELISP_NATIVE_LITERAL("EMACSDOC"), *etc);
    env->Add(RULES_ELISP_NATIVE_LITERAL("EMACSLOADPATH"), *lisp);
    env->Add(RULES_ELISP_NATIVE_LITERAL("EMACSPATH"), *libexec);
  }
  absl::StatusOr<Environment> orig_env = Environment::Current();
  if (!orig_env.ok()) return orig_env.status();
  env->Merge(*orig_env);
  if constexpr (kWindows) {
    // On Windows, Emacs doesn’t support Unicode arguments or environment
    // variables.  Check here rather than sending over garbage.
    for (const NativeString& arg : args) {
      if (const absl::Status status = CheckASCII(arg); !status.ok()) {
        return status;
      }
    }
    for (const auto& [name, value] : *env) {
      if (const absl::Status status = CheckASCII(name); !status.ok()) {
        return status;
      }
      if (const absl::Status status = CheckASCII(value); !status.ok()) {
        return status;
      }
    }
  }
  return Run(args, *env);
}

absl::StatusOr<int> Main(
    const Mode mode, const std::string_view install,
    const absl::Span<const NativeStringView> original_args) {
  return RunEmacs(BAZEL_CURRENT_REPOSITORY, mode, install, original_args);
}

}  // namespace rules_elisp
