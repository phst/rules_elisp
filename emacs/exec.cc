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
#include <cstring>
#include <filesystem>
#include <ios>
#include <iterator>
#include <map>
#include <memory>
#include <numeric>
#include <optional>
#include <ostream>
#include <regex>
#include <set>
#include <stdexcept>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

#include <spawn.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "google/protobuf/duration.pb.h"
#include "google/protobuf/repeated_field.h"
#include "google/protobuf/util/json_util.h"
#include "google/protobuf/util/time_util.h"
#include "tinyxml2.h"
#include "tools/cpp/runfiles/runfiles.h"

#include "emacs/manifest.pb.h"
#include "emacs/report.pb.h"
#include "internal/file.h"
#include "internal/random.h"

namespace phst_rules_elisp {

namespace fs = std::filesystem;
using bazel::tools::cpp::runfiles::Runfiles;
using google::protobuf::util::TimeUtil;

namespace {
class missing_runfile : public std::runtime_error {
  using std::runtime_error::runtime_error;
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

static void add_manifest(const mode mode, std::vector<std::string>& args,
                         random& random, std::optional<temp_stream>& stream) {
  if (mode == mode::direct) return;
  stream.emplace(fs::temp_directory_path(), "manifest-*.json", random);
  args.push_back("--manifest=" + stream->path().string());
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
                           std::ostream& file) {
  Manifest manifest;
  add_to_manifest(load_path, *manifest.mutable_load_path());
  add_to_manifest(load_files, *manifest.mutable_input_files());
  add_to_manifest(data_files, *manifest.mutable_input_files());
  add_to_manifest(output_files, *manifest.mutable_output_files());
  std::string json;
  const auto status =
      google::protobuf::util::MessageToJsonString(manifest, &json);
  if (!status.ok()) throw std::runtime_error(status.ToString());
  file.write(json.data(), json.size());
  file.flush();
}

static double float_seconds(
    const google::protobuf::Duration& duration) noexcept {
  return static_cast<double>(duration.seconds()) +
         static_cast<double>(duration.nanos()) / 1'000'000'000;
}

static void convert_report(std::istream& json_file, const fs::path& xml_file) {
  using iterator = std::istreambuf_iterator<char>;
  const std::string json(iterator(json_file), iterator{});
  TestReport report;
  const auto status =
      google::protobuf::util::JsonStringToMessage(json, &report);
  if (!status.ok()) {
    throw std::runtime_error("invalid JSON report: " + json + "; " +
                             status.ToString());
  }
  file stream(xml_file, file_mode::write | file_mode::create | file_mode::excl);
  const auto c_file = stream.open_c_file("wb");
  tinyxml2::XMLPrinter printer(c_file);
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
  if (std::fflush(c_file) == EOF) {
    throw std::system_error(errno, std::generic_category(), "fflush");
  }
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
  std::vector<std::string> args;
  std::optional<temp_stream> manifest;
  add_manifest(mode, args, random_, manifest);
  args.push_back("--quick");
  args.push_back("--batch");
  this->add_load_path(args, load_path);
  for (const auto& file : load_files) {
    args.push_back("--load=" + runfile(file).string());
  }
  if (manifest) {
    write_manifest(load_path, load_files, data_files, {}, manifest.value());
  }
  const auto code = this->run(emacs, args, {});
  if (manifest) manifest->close();
  return code;
}

int executor::run_test(const char* const wrapper, const mode mode,
                       const std::vector<std::filesystem::path>& load_path,
                       const std::vector<std::filesystem::path>& srcs,
                       const std::vector<std::filesystem::path>& data_files) {
  const auto emacs = runfile(wrapper);
  std::vector<std::string> args;
  std::optional<temp_stream> manifest;
  add_manifest(mode, args, random_, manifest);
  args.push_back("--quick");
  args.push_back("--batch");
  this->add_load_path(args, load_path);
  const auto runner = this->runfile("phst_rules_elisp/elisp/ert/runner.elc");
  args.push_back("--load=" + runner.string());
  args.push_back("--funcall=elisp/ert/run-batch-and-exit");
  const auto xml_output_file = this->env_var("XML_OUTPUT_FILE");
  std::optional<temp_stream> report_file;
  if (!xml_output_file.empty()) {
    const fs::path temp_dir = this->env_var("TEST_TMPDIR");
    report_file.emplace(temp_dir, "test-report-*.json", random_);
    args.push_back("--report=/:" + report_file->path().string());
  }
  for (const auto& file : srcs) {
    args.push_back(this->runfile(file).string());
  }
  if (manifest) {
    std::vector<fs::path> outputs;
    if (report_file) {
      outputs.push_back(report_file->path());
    }
    if (this->env_var("COVERAGE") == "1") {
      const auto coverage_dir = this->env_var("COVERAGE_DIR");
      if (!coverage_dir.empty()) {
        outputs.push_back(fs::path(coverage_dir) / "emacs-lisp.dat");
      }
    }
    write_manifest(load_path, srcs, data_files, outputs, manifest.value());
  }
  const int code = this->run(emacs, args, {});
  if (report_file) {
    convert_report(*report_file, xml_output_file);
    report_file->close();
  }
  if (manifest) manifest->close();
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
