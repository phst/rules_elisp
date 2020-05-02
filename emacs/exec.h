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

#ifndef PHST_RULES_ELISP_EMACS_EXEC_H
#define PHST_RULES_ELISP_EMACS_EXEC_H

#include <vector>
#include <string>

#include "absl/base/attributes.h"

namespace phst_rules_elisp {

enum class Mode { kDirect, kWrap };

ABSL_MUST_USE_RESULT int RunEmacs(const char* install_rel, int argc,
                                  const char* const* argv,
                                  const char* const* envp);

ABSL_MUST_USE_RESULT int RunBinary(const char* wrapper, Mode mode,
                                   const std::vector<std::string>& load_path,
                                   const std::vector<std::string>& load_files,
                                   const std::vector<std::string>& data_files,
                                   int argc, const char* const* argv,
                                   const char* const* envp);

ABSL_MUST_USE_RESULT int RunTest(const char* wrapper, Mode mode,
                                 const std::vector<std::string>& load_path,
                                 const std::vector<std::string>& srcs,
                                 const std::vector<std::string>& data_files,
                                 int argc, const char* const* argv,
                                 const char* const* envp);

}  // namespace phst_rules_elisp

#endif
