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

#ifndef PHST_RULES_ELISP_ELISP_PROCESS_H
#define PHST_RULES_ELISP_ELISP_PROCESS_H

#include <memory>
#include <string>
#include <vector>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#pragma GCC diagnostic ignored "-Woverflow"
#include "absl/container/flat_hash_map.h"
#include "absl/status/statusor.h"
#include "tools/cpp/runfiles/runfiles.h"
#pragma GCC diagnostic pop

#include "elisp/options.h"

namespace phst_rules_elisp {

using bazel::tools::cpp::runfiles::Runfiles;
using Environment = absl::flat_hash_map<std::string, std::string>;

Environment CopyEnv();

absl::StatusOr<std::unique_ptr<Runfiles>> CreateRunfiles(
    const std::string& argv0);

absl::StatusOr<std::string> Runfile(const Runfiles& runfiles,
                                    const std::string& rel);

absl::StatusOr<int> Run(const Argv& orig_args, const Environment& orig_env,
                        const Runfiles& runfiles, const std::string& binary,
                        const std::vector<std::string>& args,
                        const Environment& env);

}  // namespace phst_rules_elisp

#endif
