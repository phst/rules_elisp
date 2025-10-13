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

#include "elisp/private/tools/process.h"

#include <cstdlib>
#include <initializer_list>
#include <iterator>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/log/check.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/types/span.h"

#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/runfiles.h"
#include "elisp/private/tools/strings.h"
#include "elisp/private/tools/system.h"

namespace rules_elisp {

absl::StatusOr<int> RunLauncher(
    const std::string_view source_repository, const std::string_view binary,
    const CommonOptions& common_opts,
    const std::initializer_list<NativeStringView> common_args,
    const absl::Span<const NativeString> launcher_args,
    const absl::Span<const NativeStringView> original_args,
    const ExecutableKind kind) {
  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(kind, source_repository, original_args);
  if (!runfiles.ok()) return runfiles.status();
  absl::StatusOr<NativeString> resolved_binary = runfiles->Resolve(binary);
  if (!resolved_binary.ok()) return resolved_binary.status();
  std::vector<NativeString> final_args{*resolved_binary};
  final_args.insert(final_args.end(), common_args.begin(), common_args.end());
  final_args.push_back(RULES_ELISP_NATIVE_LITERAL("--wrapper=") +
                       static_cast<NativeString>(common_opts.wrapper));
  final_args.push_back(common_opts.mode == rules_elisp::ToolchainMode::kWrap
                           ? RULES_ELISP_NATIVE_LITERAL("--mode=wrap")
                           : RULES_ELISP_NATIVE_LITERAL("--mode=direct"));
  for (const NativeStringView tag : common_opts.tags) {
    final_args.push_back(RULES_ELISP_NATIVE_LITERAL("--rule-tag=") +
                         static_cast<NativeString>(tag));
  }
  for (const NativeStringView dir : common_opts.load_path) {
    final_args.push_back(RULES_ELISP_NATIVE_LITERAL("--load-directory=") +
                         static_cast<NativeString>(dir));
  }
  for (const NativeStringView file : common_opts.load_files) {
    final_args.push_back(RULES_ELISP_NATIVE_LITERAL("--load-file=") +
                         static_cast<NativeString>(file));
  }
  for (const NativeStringView file : common_opts.data_files) {
    final_args.push_back(RULES_ELISP_NATIVE_LITERAL("--data-file=") +
                         static_cast<NativeString>(file));
  }
  final_args.insert(final_args.end(), launcher_args.begin(),
                    launcher_args.end());
  final_args.push_back(RULES_ELISP_NATIVE_LITERAL("--"));
  final_args.insert(final_args.end(), original_args.begin(),
                    original_args.end());
  absl::StatusOr<Environment> merged_env = runfiles->Environ();
  if (!merged_env.ok()) return merged_env.status();
  absl::StatusOr<Environment> orig_env = Environment::Current();
  if (!orig_env.ok()) return orig_env.status();
  // We don’t want the Python launcher to change the current working directory,
  // otherwise relative filenames will be all messed up.  See
  // https://github.com/bazelbuild/bazel/issues/7190.
  orig_env->Remove(RULES_ELISP_NATIVE_LITERAL("RUN_UNDER_RUNFILES"));
  merged_env->Merge(*orig_env);
  return Run(final_args, *merged_env);
}

absl::StatusOr<int> RunEmacs(
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

}  // namespace rules_elisp
