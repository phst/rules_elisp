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

#include <map>
#include <memory>
#include <string>
#include <vector>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#include "absl/status/status.h"
#include "tools/cpp/runfiles/runfiles.h"
#pragma GCC diagnostic pop

#include "internal/random.h"
#include "internal/statusor.h"

namespace phst_rules_elisp {

enum class Mode { kDirect, kWrap };

class Executor {
 public:
  static StatusOr<Executor> Create(int argc, const char* const* argv,
                                   const char* const* envp);

  static StatusOr<Executor> CreateForTest(int argc, const char* const* argv,
                                          const char* const* envp);

  Executor(const Executor&) = delete;
  Executor(Executor&&) = default;
  Executor& operator=(const Executor&) = delete;
  Executor& operator=(Executor&&) = default;

  StatusOr<int> RunEmacs(const char* install_rel);

  StatusOr<int> RunBinary(const char* wrapper, Mode mode,
                          const std::vector<std::string>& load_path,
                          const std::vector<std::string>& load_files,
                          const std::vector<std::string>& data_files);

  StatusOr<int> RunTest(const char* wrapper, Mode mode,
                        const std::vector<std::string>& load_path,
                        const std::vector<std::string>& srcs,
                        const std::vector<std::string>& data_files);

 private:
  explicit Executor(
      int argc, const char* const* argv, const char* const* envp,
      std::unique_ptr<bazel::tools::cpp::runfiles::Runfiles> runfiles);

  StatusOr<std::string> Runfile(const std::string& rel) const;
  std::string EnvVar(const std::string& name) const noexcept;

  absl::Status AddLoadPath(std::vector<std::string>& args,
                           const std::vector<std::string>& load_path) const;

  StatusOr<int> Run(const std::string& binary,
                    const std::vector<std::string>& args,
                    const std::map<std::string, std::string>& env);

  std::vector<std::string> BuildArgs(
      const std::vector<std::string>& prefix) const;

  std::vector<std::string> BuildEnv(
      const std::map<std::string, std::string>& other) const;

  std::vector<std::string> orig_args_;
  std::map<std::string, std::string> orig_env_;
  std::unique_ptr<bazel::tools::cpp::runfiles::Runfiles> runfiles_;
  Random random_;
};

}  // namespace phst_rules_elisp

#endif
