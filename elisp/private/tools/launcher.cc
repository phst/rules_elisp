// Copyright 2020, 2021, 2024, 2025 Google LLC
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

#include "absl/base/log_severity.h"
#include "absl/container/fixed_array.h"
#include "absl/log/globals.h"
#include "absl/log/initialize.h"
#include "absl/log/log.h"
#include "absl/status/statusor.h"
#include "absl/types/span.h"

#include "elisp/private/tools/platform.h"

#if (defined RULES_ELISP_EMACS + defined RULES_ELISP_BINARY + \
     defined RULES_ELISP_TEST) != 1
#  error Incorrect launcher setup
#endif

#ifdef RULES_ELISP_EMACS
#  include "elisp/private/tools/emacs.h"
#endif

#ifdef RULES_ELISP_BINARY
#  include "elisp/private/tools/binary.h"
#endif

#ifdef RULES_ELISP_TEST
#  include "elisp/private/tools/test.h"
#endif

int
#ifdef _WIN32
    wmain
#else
    main
#endif
(int argc, rules_elisp::NativeChar** argv) {
  absl::InitializeLog();
  absl::SetStderrThreshold(absl::LogSeverityAtLeast::kWarning);
  const absl::FixedArray<rules_elisp::NativeStringView> original_args(
      argv, argv + argc);
#ifndef RULES_ELISP_EMACS
  rules_elisp::CommonOptions common_opts;
  common_opts.wrapper = RULES_ELISP_WRAPPER;
  common_opts.mode = RULES_ELISP_MODE;
  common_opts.tags = {RULES_ELISP_TAGS};
  common_opts.load_path = {RULES_ELISP_LOAD_PATH};
  common_opts.load_files = {RULES_ELISP_LOAD_FILES};
  common_opts.data_files = {RULES_ELISP_DATA_FILES};
#endif
#ifdef RULES_ELISP_BINARY
  rules_elisp::BinaryOptions binary_opts;
  binary_opts.interactive = RULES_ELISP_INTERACTIVE;
  binary_opts.input_args = {RULES_ELISP_INPUT_ARGS};
  binary_opts.output_args = {RULES_ELISP_OUTPUT_ARGS};
#endif
#ifdef RULES_ELISP_TEST
  rules_elisp::TestOptions test_opts;
  test_opts.skip_tests = {RULES_ELISP_SKIP_TESTS};
  test_opts.skip_tags = {RULES_ELISP_SKIP_TAGS};
  test_opts.module_assertions = RULES_ELISP_MODULE_ASSERTIONS;
#endif
  const absl::StatusOr<int> code = rules_elisp::Main(
#ifdef RULES_ELISP_EMACS
      RULES_ELISP_MODE, RULES_ELISP_INSTALL,
#else
      common_opts,
#endif
#ifdef RULES_ELISP_BINARY
      binary_opts,
#endif
#ifdef RULES_ELISP_TEST
      test_opts,
#endif
      original_args);
  if (!code.ok()) {
    LOG(ERROR) << code.status();
    return EXIT_FAILURE;
  }
  return *code;
}
