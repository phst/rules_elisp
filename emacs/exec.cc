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

#include "emacs/exec.h"

#include <cerrno>
#include <cstring>
#include <filesystem>
#include <iterator>
#include <map>
#include <memory>
#include <numeric>
#include <regex>
#include <set>
#include <stdexcept>
#include <string>
#include <system_error>
#include <utility>
#include <variant>
#include <vector>

#include <spawn.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "tools/cpp/runfiles/runfiles.h"

namespace phst_rules_elisp {

namespace {
class missing_runfile : public std::runtime_error {
  using std::runtime_error::runtime_error;
};
}  // namespace

namespace fs = std::filesystem;
using bazel::tools::cpp::runfiles::Runfiles;

template <typename I>
static std::string join(const I first, const I last) {
  if (first == last) return "";
  std::string init = *first;
  return std::accumulate(
      std::next(first), last, init,
      [](const std::string& a, const std::string& b) { return a + ", " + b; });
}

static std::vector<char*> pointers(std::vector<std::string>& strings) {
  std::vector<char*> ptrs;
  for (auto& s : strings) {
    if (s.find('\0') != s.npos) {
      throw std::invalid_argument(s + " contains null character");
    }
    ptrs.push_back(s.data());
  }
  ptrs.push_back(nullptr);
  return ptrs;
}

static std::map<std::string, std::string> copy_env(const char* const* envp) {
  std::map<std::string, std::string> map;
  for (const char* const* pp = envp; *pp != nullptr; ++pp) {
    const char* p = *pp;
    const char* q = std::strchr(p, '=');
    if (q != nullptr) {
      map.emplace(std::string(p, q), std::string(q + 1));
    }
  }
  return map;
}

static std::unique_ptr<Runfiles> create_runfiles(const std::string& argv0) {
  std::string error;
  std::unique_ptr<Runfiles> runfiles(Runfiles::Create(argv0, &error));
  if (runfiles == nullptr) {
    throw std::runtime_error("couldn’t create runfiles: " + error);
  }
  return runfiles;
}

static std::unique_ptr<Runfiles> create_runfiles_for_test() {
  std::string error;
  std::unique_ptr<Runfiles> runfiles(Runfiles::CreateForTest(&error));
  if (runfiles == nullptr) {
    throw std::runtime_error("couldn’t create runfiles for test: " + error);
  }
  return runfiles;
}

static fs::path get_shared_dir(const fs::path& install) {
  std::set<fs::path> dirs;
  for (const auto& entry :
       fs::directory_iterator(install / "share" / "emacs")) {
    if (entry.is_directory() &&
        std::regex_match(entry.path().filename().string(),
                         std::regex("[.0-9]+"))) {
      dirs.insert(entry);
    }
  }
  if (dirs.empty()) throw missing_runfile("no shared directory found");
  if (dirs.size() != 1) {
    throw std::runtime_error("expected exactly one shared directory, got [" +
                             join(dirs.begin(), dirs.end()) + ']');
  }
  return *dirs.begin();
}

executor::executor(int argc, const char* const* argv, const char* const* envp)
    : orig_args_(argv, argv + argc),
      orig_env_(copy_env(envp)),
      runfiles_(create_runfiles(orig_args_.at(0))) {}

executor::executor(for_test, int argc, const char* const* argv,
                   const char* const* envp)
    : orig_args_(argv, argv + argc),
      orig_env_(copy_env(envp)),
      runfiles_(create_runfiles_for_test()) {}

int executor::run_emacs(const char* install_rel) {
  const auto install = runfile(install_rel);
  const auto emacs = install / "bin" / "emacs";
  const auto shared = get_shared_dir(install);
  const auto etc = shared / "etc";
  std::map<std::string, std::string> map;
  map.emplace("EMACSDATA", etc);
  map.emplace("EMACSDOC", etc);
  map.emplace("EMACSLOADPATH", shared / "lisp");
  map.emplace("EMACSPATH", install / "libexec");
  return this->run(emacs, {}, map);
}

int executor::run_binary(const char* const wrapper,
                         const std::vector<std::filesystem::path>& load_path,
                         const std::vector<std::filesystem::path>& load_files) {
  const auto emacs = runfile(wrapper);
  std::vector<std::string> args{"--quick", "--batch"};
  this->add_load_path(args, load_path);
  for (const auto& file : load_files) {
    args.push_back("--load=" + runfile(file).string());
  }
  return this->run(emacs, args, {});
}

int executor::run_test(const char* const wrapper,
                       const std::vector<std::filesystem::path>& load_path,
                       const std::vector<std::filesystem::path>& srcs) {
  const auto emacs = runfile(wrapper);
  std::vector<std::string> args{"--quick", "--batch"};
  this->add_load_path(args, load_path);
  const auto runner = this->runfile("phst_rules_elisp/elisp/ert/runner.elc");
  args.push_back("--load=" + runner.string());
  args.push_back("--funcall=elisp/ert/run-batch-and-exit");
  for (const auto& file : srcs) {
    args.push_back(this->runfile(file).string());
  }
  return this->run(emacs, args, {});
}

fs::path executor::runfile(const fs::path& rel) const {
  const std::string str = runfiles_->Rlocation(rel.lexically_normal());
  if (str.empty()) {
    throw missing_runfile("runfile not found: " + rel.string());
  }
  return fs::canonical(str);
}

void executor::add_load_path(
    std::vector<std::string>& args,
    const std::vector<std::filesystem::path>& load_path) const {
  constexpr const char* const runfiles_elc =
      "phst_rules_elisp/elisp/runfiles/runfiles.elc";
  bool runfile_handler_installed = false;
  for (const auto& dir : load_path) {
    try {
      args.push_back("--directory=" + runfile(dir).string());
    } catch (const missing_runfile&) {
      if (!std::exchange(runfile_handler_installed, true)) {
        args.push_back("--load=" + runfile(runfiles_elc).string());
        args.push_back("--funcall=elisp/runfiles/install-handler");
      }
      args.push_back("--directory=/bazel-runfile:" + dir.string());
    }
  }
}

int executor::run(const fs::path& binary, const std::vector<std::string>& args,
                  const std::map<std::string, std::string>& env) {
  auto final_args = build_args(args);
  const auto argv = pointers(final_args);
  auto final_env = build_env(env);
  const auto envp = pointers(final_env);
  int pid;
  const int error = posix_spawn(&pid, binary.c_str(), nullptr, nullptr,
                                argv.data(), envp.data());
  if (error != 0) {
    throw std::system_error(error, std::generic_category(),
                            "posix_spawn(" + binary.string() + ')');
  }
  int wstatus;
  const int status = waitpid(pid, &wstatus, 0);
  if (status != pid) {
    throw std::system_error(errno, std::generic_category(),
                            "waitpid(" + std::to_string(pid) + ')');
  }
  return WIFEXITED(wstatus) ? WEXITSTATUS(wstatus) : 0xFF;
}

std::vector<std::string> executor::build_args(
    const std::vector<std::string>& prefix) const {
  std::vector<std::string> vec{orig_args_.at(0)};
  vec.insert(vec.end(), prefix.begin(), prefix.end());
  vec.insert(vec.end(), std::next(orig_args_.begin()), orig_args_.end());
  return vec;
}

std::vector<std::string> executor::build_env(
    const std::map<std::string, std::string>& other) const {
  const auto& pairs = runfiles_->EnvVars();
  std::map<std::string, std::string> map(pairs.begin(), pairs.end());
  map.insert(other.begin(), other.end());
  map.insert(orig_env_.begin(), orig_env_.end());
  std::vector<std::string> vec;
  for (const auto& p : map) {
    vec.push_back(p.first + '=' + p.second);
  }
  return vec;
}

}  // namespace phst_rules_elisp
