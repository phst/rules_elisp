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

#include "tools/cpp/runfiles/runfiles.h"

#include "internal/random.h"

namespace phst_rules_elisp {

struct ForTest {};

enum class Mode { kDirect, kWrap };

class Executor {
 public:
  explicit Executor(int argc, const char* const* argv, const char* const* envp);

  explicit Executor(ForTest, int argc, const char* const* argv,
                    const char* const* envp);

  int RunEmacs(const char* install_rel);

  int RunBinary(const char* wrapper, Mode mode,
                const std::vector<std::string>& load_path,
                const std::vector<std::string>& load_files,
                const std::vector<std::string>& data_files);

  int RunTest(const char* wrapper, Mode mode,
              const std::vector<std::string>& load_path,
              const std::vector<std::string>& srcs,
              const std::vector<std::string>& data_files);

 private:
  std::string Runfile(const std::string& rel) const;
  std::string EnvVar(const std::string& name) const noexcept;

  void AddLoadPath(std::vector<std::string>& args,
                   const std::vector<std::string>& load_path) const;

  int Run(const std::string& binary, const std::vector<std::string>& args,
          const std::map<std::string, std::string>& env);

  std::vector<std::string> BuildArgs(
      const std::vector<std::string>& prefix) const;

  std::vector<std::string> BuildEnv(
      const std::map<std::string, std::string>& other) const;

  const std::vector<std::string> orig_args_;
  const std::map<std::string, std::string> orig_env_;
  const std::unique_ptr<bazel::tools::cpp::runfiles::Runfiles> runfiles_;
  Random random_;
};

}  // namespace phst_rules_elisp

#endif
