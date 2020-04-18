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

#include <algorithm>
#include <cerrno>
#include <cstdio>
#include <cstring>
#include <exception>
#include <filesystem>
#include <fstream>
#include <ios>
#include <iostream>
#include <iterator>
#include <limits>
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
#include "google/protobuf/util/time_util.h"
#include "tinyxml2.h"
#include "tools/cpp/runfiles/runfiles.h"

#include "emacs/manifest.pb.h"
#include "emacs/report.pb.h"
#include "emacs/random.h"

namespace phst_rules_elisp {

namespace fs = std::filesystem;
using bazel::tools::cpp::runfiles::Runfiles;
using google::protobuf::util::TimeUtil;

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

class stream {
 public:
  explicit stream(const fs::path& path, const char* const mode)
      : file_(std::fopen(path.c_str(), mode)) {
    if (file_ == nullptr) {
      throw std::ios::failure("error opening file " + path.string() +
                              " in mode " + mode);
    }
    this->check();
  }

  ~stream() noexcept {
    // We require calling close() explicitly.
    if (file_ == nullptr) return;
    std::clog << "file still open" << std::endl;
    std::terminate();
  }

  stream(const stream&) = delete;
  stream& operator=(const stream&) = delete;

  std::FILE* file() {
    this->check();
    return file_;
  }

  void close() {
    this->check();
    const auto status = std::fclose(file_);
    file_ = nullptr;
    if (status != 0) throw std::ios::failure("error closing file");
  }

  void write(const std::string_view data) {
    this->check();
    if (std::fwrite(data.data(), 1, data.size(), file_) != data.size()) {
      throw std::ios::failure("error writing " + std::to_string(data.size()) +
                              " characters");
    }
    this->check();
  }

 private:
  void check() {
    if (file_ == nullptr) throw std::logic_error("file is already closed");
    if (std::ferror(file_) != 0) throw std::ios::failure("file error");
  }

  std::FILE* file_;
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

static std::string read_file(const fs::path& path) {
  std::ifstream stream;
  // Ensure that failures aren’t silently ignored.
  stream.exceptions(std::ios::badbit | std::ios::failbit | std::ios::eofbit);
  stream.imbue(std::locale::classic());
  // We can only open the file now, otherwise failures during open would have
  // been ignored.
  stream.open(path, std::ios::in | std::ios::binary);
  // Make sure to use unformatted input to avoid messing around with locales.
  using iterator = std::istreambuf_iterator<char>;
  const std::string result(iterator(stream), iterator{});
  // It’s crucial to call close() explicitly because the destructor swallows
  // exceptions.
  stream.close();
  return result;
}

static void write_file(const fs::path& path, const std::string_view contents) {
  // We use std::fopen, because only that can open a file in exclusive mode.
  stream stream(path, "wbx");
  stream.write(contents);
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

static double float_seconds(
    const google::protobuf::Duration& duration) noexcept {
  return static_cast<double>(duration.seconds()) +
         static_cast<double>(duration.nanos()) / 1'000'000'000;
}

static void convert_report(const fs::path& json_file, const fs::path& xml_file) {
  const auto json = read_file(json_file);
  TestReport report;
  const auto status =
      google::protobuf::util::JsonStringToMessage(json, &report);
  if (!status.ok()) {
    throw std::runtime_error("invalid JSON report: " + json + "; " +
                             status.ToString());
  }
  stream stream(xml_file, "wbx");
  tinyxml2::XMLPrinter printer(stream.file());
  // The expected format of the XML output file isn’t well-documented.
  // https://docs.bazel.build/versions/3.0.0/test-encyclopedia.html#initial-conditions
  // only states that the XML file is “ANT-like.”
  // https://llg.cubic.org/docs/junit/ and
  // https://help.catchsoftware.com/display/ET/JUnit+Format contain a bit of
  // documentation.
  printer.PushHeader(false, true);
  const auto& tests = report.tests();
  const auto total = report.tests_size();
  const auto unexpected =
      std::count_if(tests.begin(), tests.end(),
                    [](const Test& test) { return !test.expected(); });
  const auto failures =
      std::count_if(tests.begin(), tests.end(), [](const Test& test) {
        return !test.expected() && test.status() == FAILED;
      });
  const auto errors = unexpected - failures;
  const auto total_str = std::to_string(total);
  const auto failures_str = std::to_string(failures);
  const auto errors_str = std::to_string(errors);
  const auto start_time_str = TimeUtil::ToString(report.start_time());
  const auto elapsed_str = std::to_string(float_seconds(report.elapsed()));
  printer.OpenElement("testsuites");
  printer.PushAttribute("tests", total_str.c_str());
  printer.PushAttribute("time", elapsed_str.c_str());
  printer.PushAttribute("failures", failures_str.c_str());
  printer.OpenElement("testsuite");
  printer.PushAttribute("id", "0");
  printer.PushAttribute("tests", total_str.c_str());
  printer.PushAttribute("time", elapsed_str.c_str());
  printer.PushAttribute("timestamp", start_time_str.c_str());
  printer.PushAttribute("failures", failures_str.c_str());
  printer.PushAttribute("errors", errors_str.c_str());
  for (const auto& test : report.tests()) {
    printer.OpenElement("testcase");
    printer.PushAttribute("name", test.name().c_str());
    printer.PushAttribute(
        "time", std::to_string(float_seconds(test.elapsed())).c_str());
    if (!test.expected()) {
      printer.OpenElement(test.status() == FAILED ? "failure" : "error");
      printer.PushAttribute("type", test.status());
      printer.PushText(test.message().c_str());
      printer.CloseElement();
    }
    printer.CloseElement();
  }
  printer.CloseElement();
  printer.CloseElement();
  stream.close();
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
    convert_report(report_file->path(), xml_output_file);
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
