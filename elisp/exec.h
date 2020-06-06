// Copyright 2020 Google LLC
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

#ifndef PHST_RULES_ELISP_ELISP_EXEC_H
#define PHST_RULES_ELISP_ELISP_EXEC_H

#include <string>
#include <vector>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-conversion"
#include "absl/base/attributes.h"
#include "absl/container/flat_hash_set.h"
#pragma GCC diagnostic pop

namespace phst_rules_elisp {

enum class Mode { kDirect, kWrap };

struct EmacsOptions {
  std::string install_rel;
  std::vector<std::string> argv;
};

ABSL_MUST_USE_RESULT int RunEmacs(const EmacsOptions& opts);

struct CommonOptions {
  std::string wrapper;
  Mode mode;
  absl::flat_hash_set<std::string> rule_tags;
  std::vector<std::string> load_path, load_files;
  absl::flat_hash_set<std::string> data_files;
  std::vector<std::string> argv;
};

struct BinaryOptions : CommonOptions {
  absl::flat_hash_set<int> input_args, output_args;
};

ABSL_MUST_USE_RESULT int RunBinary(const BinaryOptions& opts);

struct TestOptions : CommonOptions {
  absl::flat_hash_set<std::string> skip_tests, skip_tags;
};

ABSL_MUST_USE_RESULT int RunTest(const TestOptions& opts);

}  // namespace phst_rules_elisp

#endif
