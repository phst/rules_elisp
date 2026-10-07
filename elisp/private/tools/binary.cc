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

#include "elisp/private/tools/binary.h"

#include <iterator>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/base/attributes.h"
#include "absl/log/check.h"
#include "absl/log/log.h"
#include "absl/status/status.h"
#include "absl/status/status_macros.h"
#include "absl/status/statusor.h"
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

static absl::StatusOr<std::vector<FileName>> ArgFiles(
    const absl::Span<const NativeStringView> argv,
    const std::optional<FileName>& root, std::vector<int> indices) {
  const std::optional<int> opt_argc = CastNumber<int>(argv.size());
  if (!opt_argc.has_value()) {
    return absl::InvalidArgumentError(
        absl::StrFormat("Too many command-line arguments (%d)", argv.size()));
  }
  const int argc = *opt_argc;
  CHECK_GE(argc, 0);
  std::vector<FileName> result;
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
      ABSL_ASSIGN_OR_RETURN(FileName file, FileName::FromString(arg));
      ABSL_ASSIGN_OR_RETURN(file, file.MakeAbsolute());
      // Make filenames relative if possible.
      if (root.has_value()) {
        absl::StatusOr<FileName> rel = file.MakeRelative(*root);
        if (rel.ok()) {
          file = *std::move(rel);
        } else {
          LOG(INFO) << rel.status();
        }
      }
      result.push_back(std::move(file));
    }
  }
  return result;
}

static std::optional<FileName> RunfilesDirectory(
    const Environment& env ABSL_ATTRIBUTE_LIFETIME_BOUND) {
  if (absl::StatusOr<FileName> value = FileName::FromString(
          env.Get(RULES_ELISP_NATIVE_LITERAL("RUNFILES_DIR")));
      value.ok()) {
    return std::move(*value);
  }
  if (absl::StatusOr<FileName> value = FileName::FromString(
          env.Get(RULES_ELISP_NATIVE_LITERAL("TEST_SRCDIR")));
      value.ok()) {
    return std::move(*value);
  }
  return std::nullopt;
}

absl::StatusOr<int> Main(
    const Options& opts,
    const absl::Span<const NativeStringView> original_args) {
  ABSL_ASSIGN_OR_RETURN(
      const Runfiles runfiles,
      Runfiles::Create(ExecutableKind::kBinary, BAZEL_CURRENT_REPOSITORY,
                       original_args));

  ABSL_ASSIGN_OR_RETURN(const std::string wrapper,
                        ToNarrow(opts.wrapper, Encoding::kAscii));
  ABSL_ASSIGN_OR_RETURN(const FileName emacs, runfiles.Resolve(wrapper));

  std::vector<NativeString> args = {RULES_ELISP_NATIVE_LITERAL("--quick")};
  if (!opts.interactive) {
    args.push_back(RULES_ELISP_NATIVE_LITERAL("--batch"));
  }

  ABSL_ASSIGN_OR_RETURN(const std::vector<NativeString> load_path_args,
                        LoadPathArgs(runfiles, opts.load_path));
  args.insert(args.end(), load_path_args.cbegin(), load_path_args.cend());

  for (const NativeString& file : opts.load_files) {
    ABSL_ASSIGN_OR_RETURN(const std::string narrow,
                          ToNarrow(file, Encoding::kAscii));
    ABSL_ASSIGN_OR_RETURN(const FileName abs_name, runfiles.Resolve(narrow));
    args.push_back(RULES_ELISP_NATIVE_LITERAL("--load=") + abs_name.string());
  }

  if (!original_args.empty()) {
    args.insert(args.end(), std::next(original_args.cbegin()),
                original_args.cend());
  }

  ABSL_ASSIGN_OR_RETURN(Environment env, runfiles.Environ());
  ABSL_ASSIGN_OR_RETURN(const Environment orig_env, Environment::Current());
  env.Merge(orig_env);

  // FIXME: We need this, otherwise Emacs doesn’t correctly decode its
  // command-line arguments.  But we shouldn’t set it,
  // cf. https://bazel.build/reference/test-encyclopedia#initial-conditions.
  env.Add(RULES_ELISP_NATIVE_LITERAL("LC_CTYPE"),
          RULES_ELISP_NATIVE_LITERAL("C.UTF-8"));

  const std::optional<FileName> runfiles_dir = RunfilesDirectory(env);
  ABSL_ASSIGN_OR_RETURN(const std::vector<FileName> input_files,
                        ArgFiles(original_args, runfiles_dir, opts.input_args));
  ABSL_ASSIGN_OR_RETURN(
      const std::vector<FileName> output_files,
      ArgFiles(original_args, runfiles_dir, opts.output_args));
  ABSL_ASSIGN_OR_RETURN(const ManifestFile manifest,
                        ManifestFile::Create(opts, input_files, output_files));

  std::vector<NativeString> final_args;
  manifest.AppendArgs(final_args);
  final_args.insert(final_args.end(), args.cbegin(), args.cend());

  return RunProcess(emacs, final_args, env);
}

}  // namespace rules_elisp
