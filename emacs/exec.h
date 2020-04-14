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

#include <filesystem>
#include <string>
#include <variant>
#include <vector>

#include "tools/cpp/runfiles/runfiles.h"

namespace phst_rules_elisp {

struct for_test {};

struct directory {
  std::filesystem::path path;
};

struct load {
  std::filesystem::path path;
};

using argument = std::variant<std::string, directory, load>;

class executor {
 public:
  explicit executor(int argc, const char* const* argv, const char* const* envp);

  explicit executor(for_test, int argc, const char* const* argv,
                    const char* const* envp);

  [[noreturn]] void exec_emacs(const char* install_rel);

  [[noreturn]] void exec_binary(const char* wrapper,
                                const std::vector<argument>& args);

 private:
  std::filesystem::path runfile(const std::filesystem::path& rel) const;

  [[noreturn]] void exec(const std::filesystem::path& binary,
                         const std::vector<std::string>& args,
                         const std::map<std::string, std::string>& env);

  std::vector<std::string> build_args(
      const std::vector<std::string>& prefix) const;

  std::vector<std::string> build_env(
      const std::map<std::string, std::string>& other) const;

  const std::vector<std::string> orig_args_;
  const std::map<std::string, std::string> orig_env_;
  const std::unique_ptr<bazel::tools::cpp::runfiles::Runfiles> runfiles_;
};

}  // namespace phst_rules_elisp

#endif
