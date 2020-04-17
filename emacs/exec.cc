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
#include <fstream>
#include <iterator>
#include <ios>
#include <iostream>
#include <locale>
#include <map>
#include <memory>
#include <numeric>
#include <optional>
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

#include "google/protobuf/repeated_field.h"
#include "google/protobuf/util/json_util.h"
#include "tools/cpp/runfiles/runfiles.h"

#include "emacs/manifest.pb.h"
#include "emacs/random.h"

namespace phst_rules_elisp {

namespace fs = std::filesystem;
using bazel::tools::cpp::runfiles::Runfiles;

namespace {

class missing_runfile : public std::runtime_error {
  using std::runtime_error::runtime_error;
};

class temp_file {
 public:
  explicit temp_file(fs::path path) : path_(std::move(path)) {}

  ~temp_file() noexcept {
    std::error_code code;
    if (!std::filesystem::remove(path_, code)) {
      std::clog << "error removing temporary file " << path_ << ": " << code
                << std::endl;
    }
  }

  temp_file(const temp_file&) = delete;
  temp_file& operator=(const temp_file&) = delete;

  const fs::path& path() const noexcept { return path_; }

 private:
  fs::path path_;
};

}  // namespace

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

static fs::path temp_name(const fs::path& directory, random& random) {
  for (int i = 0; i < 10; i++) {
    const auto name = directory / random.temp_name();
    if (!fs::exists(name)) return name;
  }
  throw std::runtime_error("can’t create temporary file");
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

static void write_file(const fs::path& path, const std::string_view contents) {
  std::ofstream stream;
  // Ensure that failures aren’t silently ignored.
  stream.exceptions(std::ios::badbit | std::ios::failbit | std::ios::eofbit);
  stream.imbue(std::locale::classic());
  // We can only open the file now, otherwise failures during open would have
  // been ignored.
  stream.open(path);
  // Make sure to use unformatted output to avoid messing around with locales.
  stream.write(contents.data(), contents.size());
  // It’s crucial to call close() explicitly because the destructor swallows
  // exceptions.
  stream.close();
}

static void add_manifest(const mode mode, std::optional<temp_file>& file,
                         std::vector<std::string>& args, random& random) {
  if (mode == mode::direct) return;
  const auto path = temp_name(fs::temp_directory_path(), random);
  file.emplace(path);
  args.push_back("--manifest=" + path.string());
  args.push_back("--");
}

static void add_to_manifest(
    const std::vector<fs::path>& files,
    google::protobuf::RepeatedPtrField<std::string>& field) {
  for (const auto& file : files) {
    if (file.is_absolute()) {
      throw std::invalid_argument("filename " + file.string() + " is absolute");
    }
    field.Add(file.string());
  }
}

static void write_manifest(const std::vector<fs::path>& load_path,
                           const std::vector<fs::path>& load_files,
                           const std::vector<fs::path>& data_files,
                           const std::vector<fs::path>& output_files,
                           const std::optional<temp_file>& file) {
  if (!file.has_value()) return;
  Manifest manifest;
  add_to_manifest(load_path, *manifest.mutable_load_path());
  add_to_manifest(load_files, *manifest.mutable_input_files());
  add_to_manifest(data_files, *manifest.mutable_input_files());
  add_to_manifest(output_files, *manifest.mutable_output_files());
  std::string json;
  const auto status =
      google::protobuf::util::MessageToJsonString(manifest, &json);
  if (!status.ok()) throw std::runtime_error(status.ToString());
  write_file(file->path(), json);
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

int executor::run_binary(const char* const wrapper, const mode mode,
                         const std::vector<std::filesystem::path>& load_path,
                         const std::vector<std::filesystem::path>& load_files,
                         const std::vector<std::filesystem::path>& data_files) {
  const auto emacs = runfile(wrapper);
  std::optional<temp_file> manifest;
  std::vector<std::string> args;
  add_manifest(mode, manifest, args, random_);
  args.push_back("--quick");
  args.push_back("--batch");
  this->add_load_path(args, load_path);
  for (const auto& file : load_files) {
    args.push_back("--load=" + runfile(file).string());
  }
  write_manifest(load_path, load_files, data_files, {}, manifest);
  return this->run(emacs, args, {});
}

int executor::run_test(const char* const wrapper, const mode mode,
                       const std::vector<std::filesystem::path>& load_path,
                       const std::vector<std::filesystem::path>& srcs,
                       const std::vector<std::filesystem::path>& data_files) {
  const auto emacs = runfile(wrapper);
  std::optional<temp_file> manifest;
  std::vector<std::string> args;
  add_manifest(mode, manifest, args, random_);
  args.push_back("--quick");
  args.push_back("--batch");
  this->add_load_path(args, load_path);
  const auto runner = this->runfile("phst_rules_elisp/elisp/ert/runner.elc");
  args.push_back("--load=" + runner.string());
  args.push_back("--funcall=elisp/ert/run-batch-and-exit");
  const auto xml_output_file = this->env_var("XML_OUTPUT_FILE");
  std::optional<temp_file> report_file;
  if (!xml_output_file.empty()) {
    const fs::path temp_dir = this->env_var("TEST_TMPDIR");
    report_file.emplace(temp_name(temp_dir, random_));
    args.push_back("--report=/:" + report_file->path().string());
  }
  for (const auto& file : srcs) {
    args.push_back(this->runfile(file).string());
  }
  if (manifest.has_value()) {
    std::vector<fs::path> outputs;
    if (report_file.has_value()) {
      outputs.push_back(report_file->path());
    }
    if (this->env_var("COVERAGE") == "1") {
      const auto coverage_dir = this->env_var("COVERAGE_DIR");
      if (!coverage_dir.empty()) {
        outputs.push_back(fs::path(coverage_dir) / "emacs-lisp.dat");
      }
    }
    write_manifest(load_path, srcs, data_files, outputs, manifest);
  }
  const int code = this->run(emacs, args, {});
  if (report_file.has_value()) {
    const auto converter =
        this->runfile("phst_rules_elisp/elisp/ert/write_xml_report");
    const int code = this->run(
        converter, {"--", report_file->path().string(), xml_output_file}, {});
    if (code != 0) {
      std::clog << "writing test XML report " << xml_output_file
                << " failed with code " << code << std::endl;
    }
}
  return code;
}

fs::path executor::runfile(const fs::path& rel) const {
  const std::string str = runfiles_->Rlocation(rel.lexically_normal());
  if (str.empty()) {
    throw missing_runfile("runfile not found: " + rel.string());
  }
  // Note: Don’t call fs::canonical here, because the Python stub looks for the
  // runfiles directory in the original filename.
  return fs::absolute(str);
}

std::string executor::env_var(const std::string& name) const noexcept {
  const auto it = orig_env_.find(name);
  return it == orig_env_.end() ? std::string() : it->second;
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
