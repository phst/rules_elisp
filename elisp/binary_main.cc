// Copyright 2020, 2021, 2024 Google LLC
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

#include <vector>

#include "elisp/binary.h"

int PHST_RULES_ELISP_MAIN(int argc, phst_rules_elisp::NativeChar** argv) {
  using phst_rules_elisp::NativeString;
  using phst_rules_elisp::RunBinary;
  std::vector<NativeString> args = {
    PHST_RULES_ELISP_ARGS,
    PHST_RULES_ELISP_NATIVE_LITERAL("--")
  };
  args.insert(args.end(), argv, argv + argc);
  return RunBinary(argc == 0 ? NativeString() : argv[0], args);
}

// Local Variables:
// mode: c++
// End:
