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
#include "absl/meta/type_traits.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/types/span.h"

#include "elisp/private/tools/platform.h"

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
  const absl::StatusOr<int> code = rules_elisp::Main(
#ifdef RULES_ELISP_EMACS
      RULES_ELISP_MODE, RULES_ELISP_INSTALL,
#else
      {
          RULES_ELISP_WRAPPER,
          RULES_ELISP_MODE,
          {RULES_ELISP_TAGS},
          {RULES_ELISP_LOAD_PATH},
          {RULES_ELISP_LOAD_FILES},
          {RULES_ELISP_DATA_FILES},
      },
#endif
#ifdef RULES_ELISP_BINARY
      {
          RULES_ELISP_INTERACTIVE,
          {RULES_ELISP_INPUT_ARGS},
          {RULES_ELISP_OUTPUT_ARGS},
      },
#endif
#ifdef RULES_ELISP_TEST
      {
          {RULES_ELISP_SKIP_TESTS},
          {RULES_ELISP_SKIP_TAGS},
          RULES_ELISP_MODULE_ASSERTIONS,
      },
#endif
      original_args);
  if (!code.ok()) {
    LOG(ERROR) << code.status();
    return EXIT_FAILURE;
  }
  return *code;
}
