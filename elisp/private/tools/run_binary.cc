// Copyright 2021-2025 Google LLC
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

#include <cstdlib>
#include <iterator>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/base/attributes.h"
#include "absl/base/log_severity.h"
#include "absl/container/fixed_array.h"
#include "absl/log/check.h"
#include "absl/log/globals.h"
#include "absl/log/initialize.h"
#include "absl/log/log.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/numbers.h"
#include "absl/strings/str_format.h"
#include "absl/types/span.h"

#include "elisp/private/tools/load.h"
#include "elisp/private/tools/manifest.h"
#include "elisp/private/tools/numeric.h"
#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/runfiles.h"
#include "elisp/private/tools/system.h"

namespace rules_elisp {

[[nodiscard]] static NativeStringView RemovePrefix(
    const NativeStringView string, const NativeStringView prefix) {
  const NativeStringView::size_type n = prefix.size();
  return n <= string.size() && string.substr(0, n) == prefix ? string.substr(n)
                                                             : string;
}

static absl::StatusOr<std::vector<NativeString>> ArgFiles(
    const absl::Span<const NativeStringView> argv, const NativeStringView root,
    std::vector<int> indices) {
  const std::optional<int> opt_argc = CastNumber<int>(argv.size());
  if (!opt_argc.has_value()) {
    return absl::InvalidArgumentError(
        absl::StrFormat("Too many command-line arguments (%d)", argv.size()));
  }
  const int argc = *opt_argc;
  CHECK_GE(argc, 0);
  std::vector<NativeString> result;
  absl::c_sort(indices);
  for (int i : indices) {
    if (i < 0) i += argc;
    if (0 < i && i < argc) {
      const std::optional<NativeStringView::size_type> j =
          CastNumber<NativeStringView::size_type>(i);
      if (!j.has_value()) {
        return absl::InvalidArgumentError(
            absl::StrFormat("Argument index %d too large", i));
      }
      // File arguments are often quoted so that Emacs doesn’t interpret
      // them as special filenames.  Unquote them first.
      const NativeStringView arg =
          RemovePrefix(argv[*j], RULES_ELISP_NATIVE_LITERAL("/:"));
      absl::StatusOr<NativeString> file = MakeAbsolute(arg);
      if (!file.ok()) return file.status();
      // Make filenames relative if possible.
      if (!root.empty()) {
        const absl::StatusOr<NativeString> rel = MakeRelative(*file, root);
        if (rel.ok()) {
          file = rel;
        } else {
          LOG(INFO) << rel.status();
        }
      }
      result.push_back(*file);
    }
  }
  return result;
}

static NativeStringView RunfilesDirectory(
    const Environment& env ABSL_ATTRIBUTE_LIFETIME_BOUND) {
  const NativeStringView dir =
      env.Get(RULES_ELISP_NATIVE_LITERAL("RUNFILES_DIR"));
  return dir.empty() ? env.Get(RULES_ELISP_NATIVE_LITERAL("TEST_SRCDIR")) : dir;
}

[[nodiscard]] static bool ConsumePrefix(NativeStringView& view,
                                        const NativeStringView prefix) {
  const auto n = prefix.length();
  if (view.length() >= n && view.substr(0, n) == prefix) {
    view.remove_prefix(n);
    return true;
  }
  return false;
}

static absl::StatusOr<int> ParseInt(const NativeStringView arg) {
  const absl::StatusOr<std::string> narrow = ToNarrow(arg, Encoding::kAscii);
  if (!narrow.ok()) return narrow.status();
  int i;
  const bool ok = absl::SimpleAtoi(*narrow, &i);
  if (!ok) {
    return absl::InvalidArgumentError(
        absl::StrFormat("Cannot parse integer argument %s", *narrow));
  }
  return i;
}

static absl::StatusOr<int> RunBinary(absl::Span<const NativeStringView> argv) {
  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(ExecutableKind::kBinary, BAZEL_CURRENT_REPOSITORY, argv);
  if (!runfiles.ok()) return runfiles.status();

  if (argv.empty()) return absl::InvalidArgumentError("Empty argument vector");
  argv.remove_prefix(1);

  Options opts;
  bool interactive = false;
  std::vector<int> input_args;
  std::vector<int> output_args;
  while (!argv.empty()) {
    NativeStringView arg = argv.front();
    if (arg.empty() || arg.front() != RULES_ELISP_NATIVE_LITERAL('-')) break;
    argv.remove_prefix(1);
    if (arg == RULES_ELISP_NATIVE_LITERAL("--")) break;
    if (ConsumePrefix(arg, RULES_ELISP_NATIVE_LITERAL("--wrapper="))) {
      opts.wrapper = arg;
    } else if (arg == RULES_ELISP_NATIVE_LITERAL("--mode=direct")) {
      opts.mode = ToolchainMode::kDirect;
    } else if (arg == RULES_ELISP_NATIVE_LITERAL("--mode=wrap")) {
      opts.mode = ToolchainMode::kWrap;
    } else if (ConsumePrefix(arg, RULES_ELISP_NATIVE_LITERAL("--rule-tag="))) {
      opts.tags.emplace_back(arg);
    } else if (ConsumePrefix(arg,
                             RULES_ELISP_NATIVE_LITERAL("--load-directory="))) {
      opts.load_path.emplace_back(arg);
    } else if (ConsumePrefix(arg, RULES_ELISP_NATIVE_LITERAL("--load-file="))) {
      opts.load_files.emplace_back(arg);
    } else if (ConsumePrefix(arg, RULES_ELISP_NATIVE_LITERAL("--data-file="))) {
      opts.data_files.emplace_back(arg);
    } else if (arg == RULES_ELISP_NATIVE_LITERAL("--interactive")) {
      interactive = true;
    } else if (ConsumePrefix(arg, RULES_ELISP_NATIVE_LITERAL("--input-arg="))) {
      const absl::StatusOr<int> i = ParseInt(arg);
      if (!i.ok()) return i.status();
      input_args.push_back(*i);
    } else if (ConsumePrefix(arg,
                             RULES_ELISP_NATIVE_LITERAL("--output-arg="))) {
      const absl::StatusOr<int> i = ParseInt(arg);
      if (!i.ok()) return i.status();
      output_args.push_back(*i);
    } else {
      return absl::InvalidArgumentError(
          absl::StrFormat("Invalid command-line argument %s", arg));
    }
  }

  const absl::StatusOr<std::string> wrapper =
      ToNarrow(opts.wrapper, Encoding::kAscii);
  if (!wrapper.ok()) return wrapper.status();
  const absl::StatusOr<NativeString> emacs = runfiles->Resolve(*wrapper);
  if (!emacs.ok()) return emacs.status();

  std::vector<NativeString> args = {RULES_ELISP_NATIVE_LITERAL("--quick")};
  if (!interactive) args.push_back(RULES_ELISP_NATIVE_LITERAL("--batch"));

  const absl::StatusOr<std::vector<NativeString>> load_path_args =
      LoadPathArgs(*runfiles, opts.load_path);
  if (!load_path_args.ok()) return load_path_args.status();
  args.insert(args.end(), load_path_args->cbegin(), load_path_args->cend());

  for (const NativeString& file : opts.load_files) {
    const absl::StatusOr<std::string> narrow = ToNarrow(file, Encoding::kAscii);
    if (!narrow.ok()) return narrow.status();
    const absl::StatusOr<NativeString> abs_name = runfiles->Resolve(*narrow);
    if (!abs_name.ok()) return abs_name.status();
    args.push_back(RULES_ELISP_NATIVE_LITERAL("--load=") + *abs_name);
  }

  if (!argv.empty()) {
    args.insert(args.end(), std::next(argv.cbegin()), argv.cend());
  }

  absl::StatusOr<Environment> env = runfiles->Environ();
  if (!env.ok()) return env.status();
  const absl::StatusOr<Environment> orig_env = Environment::Current();
  if (!orig_env.ok()) return orig_env.status();
  env->Merge(*orig_env);

  // FIXME: We need this, otherwise Emacs doesn’t correctly decode its
  // command-line arguments.  But we shouldn’t set it,
  // cf. https://bazel.build/reference/test-encyclopedia#initial-conditions.
  env->Add(RULES_ELISP_NATIVE_LITERAL("LC_CTYPE"),
           RULES_ELISP_NATIVE_LITERAL("C.UTF-8"));

  const NativeStringView runfiles_dir = RunfilesDirectory(*env);
  const absl::StatusOr<std::vector<NativeString>> input_files =
      ArgFiles(argv, runfiles_dir, std::move(input_args));
  if (!input_files.ok()) return input_files.status();
  const absl::StatusOr<std::vector<NativeString>> output_files =
      ArgFiles(argv, runfiles_dir, std::move(output_args));
  if (!output_files.ok()) return output_files.status();

  const absl::StatusOr<ManifestFile> manifest =
      ManifestFile::Create(opts, *input_files, *output_files);
  if (!manifest.ok()) return manifest.status();

  std::vector<NativeString> final_args = {*emacs};
  manifest->AppendArgs(final_args);
  final_args.insert(final_args.end(), args.cbegin(), args.cend());

  return Run(final_args, *env);
}

}  // namespace rules_elisp

int RULES_ELISP_MAIN(int argc, rules_elisp::NativeChar** argv) {
  absl::InitializeLog();
  absl::SetStderrThreshold(absl::LogSeverityAtLeast::kWarning);
  const absl::FixedArray<rules_elisp::NativeStringView> args(argv, argv + argc);
  const absl::StatusOr<int> code = rules_elisp::RunBinary(args);
  if (!code.ok()) {
    LOG(ERROR) << code.status();
    return EXIT_FAILURE;
  }
  return *code;
}
