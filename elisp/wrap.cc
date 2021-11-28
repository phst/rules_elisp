// Copyright 2021 Google LLC
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
#include <iostream>
#include <vector>

#include "elisp/platform.h"
#include "elisp/process.h"

using phst_rules_elisp::NativeChar;
using phst_rules_elisp::NativeString;

int PHST_RULES_ELISP_MAIN(int argc, NativeChar** argv) {
  using phst_rules_elisp::Runfiles;
  using phst_rules_elisp::Run;
  if (argc < 1) {
    std::cerr << "Too few command-line arguments" << std::endl;
    return EXIT_FAILURE;
  }
  const absl::StatusOr<Runfiles> runfiles = Runfiles::Create(argv[0]);
  if (!runfiles.ok()) {
    std::cerr << runfiles.status() << std::endl;
    return EXIT_FAILURE;
  }
  std::string name = "phst_rules_elisp/tests/wrap/wrap";
#ifdef PHST_RULES_ELISP_WINDOWS
  name += ".exe";
#endif
  const absl::StatusOr<NativeString> program = runfiles->Resolve(name);
  if (!program.ok()) {
    std::cerr << runfiles.status() << std::endl;
    return EXIT_FAILURE;
  }
  std::vector<NativeString> args = {*program};
  args.insert(args.end(), argv + 1, argv + argc);
  const absl::StatusOr<int> result = Run(args, *runfiles);
  if (!result.ok()) {
    std::cerr << result.status() << std::endl;
    return EXIT_FAILURE;
  }
  return *result;
}
