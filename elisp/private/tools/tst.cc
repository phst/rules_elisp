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

#include "elisp/private/tools/tst.h"

#include <cstdint>
#include <fstream>
#include <ios>
#include <locale>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "absl/log/log.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/numbers.h"
#include "absl/strings/str_format.h"
#include "absl/time/clock.h"
#include "absl/time/time.h"
#include "absl/types/span.h"

#include "elisp/private/tools/load.h"
#include "elisp/private/tools/manifest.h"
#include "elisp/private/tools/numeric.h"
#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/runfiles.h"
#include "elisp/private/tools/strings.h"
#include "elisp/private/tools/system.h"

namespace rules_elisp {

static absl::StatusOr<NativeString> QuoteArg(const NativeStringView arg) {
  if constexpr (!kWindows) return NativeString(arg);
  const absl::StatusOr<std::string> utf8 = ToNarrow(arg, Encoding::kUtf8);
  if (!utf8.ok()) return utf8.status();
  return ToNative(PercentEncode(*utf8), Encoding::kAscii);
}

// Try to look up inaccessible files in the coverage manifest as runfiles.
//
// We do this here so that the Emacs Lisp code doesn’t have to depend on the
// runfiles library.
static absl::Status FixCoverageManifest(const NativeStringView manifest_file,
                                        const Runfiles& runfiles) {
  if (manifest_file.empty()) {
    return absl::InvalidArgumentError("Empty manifest filename");
  }
  if (ContainsNull(manifest_file)) {
    return absl::InvalidArgumentError(absl::StrFormat(
        "Manifest file %s contains null character", manifest_file));
  }
  std::vector<std::string> files;
  {
    std::ifstream stream(NativeString(manifest_file),
                         std::ios::in | std::ios::binary);
    stream.imbue(std::locale::classic());
    if (!stream.is_open() || !stream.good()) {
      return absl::FailedPreconditionError(absl::StrFormat(
          "Cannot open coverage manifest %s for reading", manifest_file));
    }
    std::string line;
    while (std::getline(stream, line)) files.push_back(std::move(line));
    if (stream.bad()) {
      return absl::FailedPreconditionError(absl::StrFormat(
          "Reading coverage manifest %s failed", manifest_file));
    }
  }
  bool edited = false;
  for (std::string& file : files) {
    const absl::StatusOr<NativeString> native = ToNative(file, Encoding::kUtf8);
    const absl::StatusOr<FileName> native_name =
        native.ok() ? FileName::FromString(*native) : native.status();
    if (!native_name.ok() || IsAbsolute(native_name->string()) ||
        FileExists(*native_name)) {
      continue;
    }
    const absl::StatusOr<FileName> resolved = runfiles.Resolve(file);
    if (!resolved.ok() || !FileExists(*resolved)) continue;
    const absl::StatusOr<std::string> narrow =
        ToNarrow(resolved->string(), Encoding::kUtf8);
    if (!narrow.ok()) continue;
    file = std::move(*narrow);
    edited = true;
  }
  if (!edited) return absl::OkStatus();

  std::ofstream stream(NativeString(manifest_file),
                       std::ios::out | std::ios::trunc | std::ios::binary);
  stream.imbue(std::locale::classic());
  if (!stream.is_open() || !stream.good()) {
    return absl::FailedPreconditionError(absl::StrFormat(
        "Cannot open coverage manifest %s for writing", manifest_file));
  }
  for (const std::string& file : files) {
    const std::optional<std::streamsize> length =
        CastNumber<std::streamsize>(file.length());
    if (!length.has_value()) {
      return absl::DataLossError(
          absl::StrFormat("Line too long (%d bytes)", file.length()));
    }
    stream.write(file.data(), *length);
    stream.put('\n');
  }
  stream.flush();
  if (!stream.good()) {
    return absl::DataLossError(
        absl::StrFormat("Writing coverage manifest %s failed", manifest_file));
  }
  return absl::OkStatus();
}

absl::StatusOr<int> Main(const Options& opts,
                         absl::Span<const NativeStringView> original_args) {
  const absl::StatusOr<Runfiles> runfiles = Runfiles::Create(
      ExecutableKind::kBinary, BAZEL_CURRENT_REPOSITORY, original_args);
  if (!runfiles.ok()) return runfiles.status();

  const absl::StatusOr<std::string> wrapper =
      ToNarrow(opts.wrapper, Encoding::kAscii);
  if (!wrapper.ok()) return wrapper.status();
  const absl::StatusOr<FileName> emacs = runfiles->Resolve(*wrapper);
  if (!emacs.ok()) return emacs.status();

  std::vector<NativeString> emacs_args = {
      RULES_ELISP_NATIVE_LITERAL("--quick"),
      RULES_ELISP_NATIVE_LITERAL("--batch"),
      RULES_ELISP_NATIVE_LITERAL("--no-build-details"),
  };
  if (opts.module_assertions) {
    emacs_args.push_back(RULES_ELISP_NATIVE_LITERAL("--module-assertions"));
  }

  const absl::StatusOr<std::vector<NativeString>> load_path_args =
      LoadPathArgs(*runfiles, opts.load_path);
  if (!load_path_args.ok()) return load_path_args.status();
  emacs_args.insert(emacs_args.end(), load_path_args->cbegin(),
                    load_path_args->cend());

  const absl::StatusOr<FileName> run_tst_elc =
      runfiles->Resolve(RULES_ELISP_RUN_TST_ELC);
  if (!run_tst_elc.ok()) return run_tst_elc.status();
  emacs_args.push_back(RULES_ELISP_NATIVE_LITERAL("--load=") +
                       run_tst_elc->string());

  for (const NativeString& file : opts.load_files) {
    const absl::StatusOr<std::string> narrow = ToNarrow(file, Encoding::kAscii);
    if (!narrow.ok()) return narrow.status();
    const absl::StatusOr<FileName> abs_name = runfiles->Resolve(*narrow);
    if (!abs_name.ok()) return abs_name.status();
    const absl::StatusOr<NativeString> quoted = QuoteArg(abs_name->string());
    if (!quoted.ok()) return quoted.status();
    emacs_args.push_back(RULES_ELISP_NATIVE_LITERAL("--test-source=") +
                         *quoted);
  }
  for (const NativeString& test : opts.skip_tests) {
    const absl::StatusOr<NativeString> quoted = QuoteArg(test);
    if (!quoted.ok()) return quoted.status();
    emacs_args.push_back(RULES_ELISP_NATIVE_LITERAL("--skip-test=") + *quoted);
  }
  for (const NativeString& tag : opts.skip_tags) {
    const absl::StatusOr<NativeString> quoted = QuoteArg(tag);
    if (!quoted.ok()) return quoted.status();
    emacs_args.push_back(RULES_ELISP_NATIVE_LITERAL("--skip-tag=") + *quoted);
  }

  emacs_args.push_back(RULES_ELISP_NATIVE_LITERAL("--"));

  if (!original_args.empty()) {
    original_args.remove_prefix(1);
    for (const NativeStringView arg : original_args) {
      const absl::StatusOr<NativeString> quoted = QuoteArg(arg);
      if (!quoted.ok()) return quoted.status();
      emacs_args.push_back(*quoted);
    }
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

  std::vector<NativeString> inputs;
  std::vector<NativeString> outputs;
  const NativeStringView report_file =
      env->Get(RULES_ELISP_NATIVE_LITERAL("XML_OUTPUT_FILE"));
  if (!report_file.empty()) outputs.emplace_back(report_file);
  if (env->Get(RULES_ELISP_NATIVE_LITERAL("COVERAGE")) ==
      RULES_ELISP_NATIVE_LITERAL("1")) {
    const NativeStringView coverage_manifest =
        env->Get(RULES_ELISP_NATIVE_LITERAL("COVERAGE_MANIFEST"));
    if (!coverage_manifest.empty()) {
      const absl::Status status =
          FixCoverageManifest(coverage_manifest, *runfiles);
      if (!status.ok()) return status;
      inputs.emplace_back(coverage_manifest);
    }
    const NativeStringView coverage_dir =
        env->Get(RULES_ELISP_NATIVE_LITERAL("COVERAGE_DIR"));
    if (!coverage_dir.empty()) {
      outputs.push_back(NativeString(coverage_dir) + kSeparator +
                        RULES_ELISP_NATIVE_LITERAL("emacs-lisp.dat"));
    }
  }

  RunOptions run_opts;
  if constexpr (kWindows) {
    // On Windows, the Bazel test runner doesn’t gracefully kill the test
    // process, see https://github.com/bazelbuild/bazel/issues/12684.  We work
    // around this by creating a new process group and sending CTRL + BREAK
    // slightly before Bazel kills us.
    const NativeStringView timeout_str =
        env->Get(RULES_ELISP_NATIVE_LITERAL("TEST_TIMEOUT"));
    if (!timeout_str.empty()) {
      const absl::StatusOr<std::string> narrow =
          ToNarrow(timeout_str, Encoding::kAscii);
      if (!narrow.ok()) return narrow.status();
      std::uint64_t seconds;
      if (!absl::SimpleAtoi(*narrow, &seconds)) {
        return absl::InvalidArgumentError(
            absl::StrFormat("Invalid TEST_TIMEOUT %s", timeout_str));
      }
      // Lower the timeout to account for infrastructure overhead.
      const absl::Duration timeout = absl::Seconds(seconds) - absl::Seconds(2);
      run_opts.deadline = absl::Now() + timeout;
    }
  }

  const absl::StatusOr<ManifestFile> manifest =
      ManifestFile::Create(opts, inputs, outputs);
  if (!manifest.ok()) return manifest.status();

  std::vector<NativeString> final_args;
  manifest->AppendArgs(final_args);
  final_args.insert(final_args.end(), emacs_args.cbegin(), emacs_args.cend());

  const absl::StatusOr<int> result =
      Run(emacs->string(), final_args, *env, run_opts);

  if (absl::IsDeadlineExceeded(result.status())) {
    LOG(INFO) << "waiting for Bazel to kill this process";
    // We want timeouts to be reflected as actual timeout results in Bazel, so
    // we force a Bazel-level timeout by sleeping for a long time.
    absl::SleepFor(absl::Seconds(20));
    // If Bazel hasn’t killed us, exit anyway.
    LOG(WARNING) << "Bazel failed to kill this process";
    return 0xFF;
  }
  return result;
}

}  // namespace rules_elisp
