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
    const absl::Span<const NativeStringView> argv, const NativeStringView root,
    std::vector<int> indices) {
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
      absl::StatusOr<FileName> name = FileName::FromString(*file);
      if (!name.ok()) return name.status();
      result.push_back(*std::move(name));
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

absl::StatusOr<int> Main(
    const Options& opts,
    const absl::Span<const NativeStringView> original_args) {
  const absl::StatusOr<Runfiles> runfiles = Runfiles::Create(
      ExecutableKind::kBinary, BAZEL_CURRENT_REPOSITORY, original_args);
  if (!runfiles.ok()) return runfiles.status();

  const absl::StatusOr<std::string> wrapper =
      ToNarrow(opts.wrapper, Encoding::kAscii);
  if (!wrapper.ok()) return wrapper.status();
  const absl::StatusOr<FileName> emacs = runfiles->Resolve(*wrapper);
  if (!emacs.ok()) return emacs.status();

  std::vector<NativeString> args = {RULES_ELISP_NATIVE_LITERAL("--quick")};
  if (!opts.interactive) {
    args.push_back(RULES_ELISP_NATIVE_LITERAL("--batch"));
  }

  const absl::StatusOr<std::vector<NativeString>> load_path_args =
      LoadPathArgs(*runfiles, opts.load_path);
  if (!load_path_args.ok()) return load_path_args.status();
  args.insert(args.end(), load_path_args->cbegin(), load_path_args->cend());

  for (const NativeString& file : opts.load_files) {
    const absl::StatusOr<std::string> narrow = ToNarrow(file, Encoding::kAscii);
    if (!narrow.ok()) return narrow.status();
    const absl::StatusOr<FileName> abs_name = runfiles->Resolve(*narrow);
    if (!abs_name.ok()) return abs_name.status();
    args.push_back(RULES_ELISP_NATIVE_LITERAL("--load=") + abs_name->string());
  }

  if (!original_args.empty()) {
    args.insert(args.end(), std::next(original_args.cbegin()),
                original_args.cend());
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
  const absl::StatusOr<std::vector<FileName>> input_files =
      ArgFiles(original_args, runfiles_dir, opts.input_args);
  if (!input_files.ok()) return input_files.status();
  const absl::StatusOr<std::vector<FileName>> output_files =
      ArgFiles(original_args, runfiles_dir, opts.output_args);
  if (!output_files.ok()) return output_files.status();

  const absl::StatusOr<ManifestFile> manifest =
      ManifestFile::Create(opts, *input_files, *output_files);
  if (!manifest.ok()) return manifest.status();

  std::vector<NativeString> final_args;
  manifest->AppendArgs(final_args);
  final_args.insert(final_args.end(), args.cbegin(), args.cend());

  return Run(emacs->string(), final_args, *env);
}

}  // namespace rules_elisp
