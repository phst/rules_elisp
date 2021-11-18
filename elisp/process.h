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

namespace phst_rules_elisp {

using Environment = absl::flat_hash_map<std::string, std::string>;

class Runfiles final {
 public:
  static absl::StatusOr<Runfiles> Create(const std::string& argv0);
  static absl::StatusOr<Runfiles> CreateForTest();
  absl::StatusOr<std::string> Resolve(const std::string& name) const;
  Environment Environment() const;

 private:
  using Impl = bazel::tools::cpp::runfiles::Runfiles;
  explicit Runfiles(std::unique_ptr<Impl> impl);
  std::unique_ptr<Impl> impl_;
};

absl::StatusOr<int> Run(const std::string& binary,
                        const std::vector<std::string>& args,
                        const Runfiles& runfiles);

}  // namespace phst_rules_elisp

#endif
