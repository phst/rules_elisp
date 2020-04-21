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

struct for_test {};

enum class mode { direct, wrap };

class executor {
 public:
  explicit executor(int argc, const char* const* argv, const char* const* envp);

  explicit executor(for_test, int argc, const char* const* argv,
                    const char* const* envp);

  int run_emacs(const char* install_rel);

  int run_binary(const char* wrapper, mode mode,
                 const std::vector<std::string>& load_path,
                 const std::vector<std::string>& load_files,
                 const std::vector<std::string>& data_files);

  int run_test(const char* wrapper, mode mode,
               const std::vector<std::string>& load_path,
               const std::vector<std::string>& srcs,
               const std::vector<std::string>& data_files);

 private:
  std::string runfile(const std::string& rel) const;
  std::string env_var(const std::string& name) const noexcept;

  void add_load_path(std::vector<std::string>& args,
                     const std::vector<std::string>& load_path) const;

  int run(const std::string& binary,
          const std::vector<std::string>& args,
          const std::map<std::string, std::string>& env);

  std::vector<std::string> build_args(
      const std::vector<std::string>& prefix) const;

  std::vector<std::string> build_env(
      const std::map<std::string, std::string>& other) const;

  const std::vector<std::string> orig_args_;
  const std::map<std::string, std::string> orig_env_;
  const std::unique_ptr<bazel::tools::cpp::runfiles::Runfiles> runfiles_;
  random random_;
};

}  // namespace phst_rules_elisp

#endif
