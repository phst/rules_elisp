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
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "tools/cpp/runfiles/runfiles.h"
#pragma GCC diagnostic pop

#include "elisp/algorithm.h"
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
  ASSIGN_OR_RETURN(const auto program,
                   Runfile(*runfiles, "phst_rules_elisp/elisp/test_py"));
  std::vector<std::string> args = {absl::StrCat("--wrapper=", opts.wrapper)};
  switch (opts.mode) {
  case Mode::kDirect:
    args.push_back("--mode=direct");
    break;
  case Mode::kWrap:
    args.push_back("--mode=wrap");
    break;
  }
  for (const std::string& tag : Sort(opts.rule_tags)) {
    args.push_back(absl::StrCat("--rule-tag=", tag));
  }
  for (const std::string& dir : opts.load_path) {
    args.push_back(absl::StrCat("--load-directory=", dir));
  }
  for (const std::string& file : opts.load_files) {
    args.push_back(absl::StrCat("--load-file=", file));
  }
  for (const std::string& file : Sort(opts.data_files)) {
    args.push_back(absl::StrCat("--data-file=", file));
  }
  for (const std::string& test : Sort(opts.skip_tests)) {
    args.push_back(absl::StrCat("--skip-test=", test));
  }
  for (const std::string& tag : Sort(opts.skip_tags)) {
    args.push_back(absl::StrCat("--skip-tag=", tag));
  }
  args.push_back("--");
  args.push_back(opts.argv.at(0));
  return Run(opts, orig_env, *runfiles, program, args);
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
