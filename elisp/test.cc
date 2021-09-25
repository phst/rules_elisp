// Copyright 2020, 2021 Google LLC
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

#include "elisp/test.h"

#include <cstdlib>
#include <iostream>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#pragma GCC diagnostic ignored "-Woverflow"
#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "absl/random/random.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "tools/cpp/runfiles/runfiles.h"
#pragma GCC diagnostic pop

#include "elisp/algorithm.h"
#include "elisp/file.h"
#include "elisp/load.h"
#include "elisp/manifest.h"
#include "elisp/process.h"
#include "elisp/status.h"

namespace phst_rules_elisp {

static absl::StatusOr<std::unique_ptr<Runfiles>> CreateRunfilesForTest() {
  std::string error;
  std::unique_ptr<Runfiles> runfiles(Runfiles::CreateForTest(&error));
  if (runfiles == nullptr) {
    return absl::FailedPreconditionError(
        absl::StrCat("couldn’t create runfiles for test: ", error));
  }
  return std::move(runfiles);
}

static absl::StatusOr<int> RunTestImpl(const TestOptions& opts) {
  const auto orig_env = CopyEnv();
  ASSIGN_OR_RETURN(const auto runfiles, CreateRunfilesForTest());
  ASSIGN_OR_RETURN(const auto emacs, Runfile(*runfiles, opts.wrapper));
  std::vector<std::string> args;
  absl::BitGen random;
  ASSIGN_OR_RETURN(auto manifest, AddManifest(opts.mode, args, random));
  args.push_back("--quick");
  args.push_back("--batch");
  args.push_back("--module-assertions");
  RETURN_IF_ERROR(AddLoadPath(*runfiles, args, opts.load_path));
  ASSIGN_OR_RETURN(const auto runner,
                   Runfile(*runfiles, "phst_rules_elisp/elisp/ert/runner.elc"));
  args.push_back(absl::StrCat("--load=", runner));
  // Note that using equals signs for "--test-source, --skip-test, and
  // --skip-tag doesn’t work.
  for (const auto& file : opts.load_files) {
    ASSIGN_OR_RETURN(const auto abs, Runfile(*runfiles, file));
    args.push_back("--test-source");
    args.push_back(absl::StrCat("/:", abs));
  }
  for (const auto& test : Sort(opts.skip_tests)) {
    args.push_back("--skip-test");
    args.push_back(test);
  }
  for (const auto& tag : Sort(opts.skip_tags)) {
    args.push_back("--skip-tag");
    args.push_back(tag);
  }
  args.push_back("--funcall=elisp/ert/run-batch-and-exit");
  if (manifest) {
    std::vector<std::string> inputs, outputs;
    const auto report_file = Find(orig_env, "XML_OUTPUT_FILE");
    if (!report_file.empty()) {
      outputs.push_back(report_file);
    }
    if (Find(orig_env, "COVERAGE") == "1") {
      std::string coverage_manifest = Find(orig_env, "COVERAGE_MANIFEST");
      if (!coverage_manifest.empty()) {
        inputs.push_back(std::move(coverage_manifest));
      }
      const auto coverage_dir = Find(orig_env, "COVERAGE_DIR");
      if (!coverage_dir.empty()) {
        outputs.push_back(JoinPath(coverage_dir, "emacs-lisp.dat"));
      }
    }
    RETURN_IF_ERROR(
        WriteManifest(opts, std::move(inputs), outputs, manifest.value()));
  }
  ASSIGN_OR_RETURN(const auto code,
                   Run(opts, orig_env, *runfiles, emacs, args, {}));
  if (manifest) RETURN_IF_ERROR(manifest->Close());
  return code;
}

int RunTest(const TestOptions& opts) {
  const auto status_or_code = RunTestImpl(opts);
  if (!status_or_code.ok()) {
    std::clog << status_or_code.status() << std::endl;
    return EXIT_FAILURE;
  }
  return status_or_code.value();
}

}  // namespace phst_rules_elisp
