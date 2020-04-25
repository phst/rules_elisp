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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#include "absl/strings/str_join.h"
#include "absl/types/optional.h"
#include "absl/utility/utility.h"
#include "google/protobuf/duration.pb.h"
#include "google/protobuf/repeated_field.h"
#include "google/protobuf/util/json_util.h"
#include "google/protobuf/util/time_util.h"
#include "tinyxml2.h"
#include "tools/cpp/runfiles/runfiles.h"
#pragma GCC diagnostic pop

#include "emacs/manifest.pb.h"
#include "emacs/report.pb.h"
#include "internal/file.h"
#include "internal/int.h"
#include "internal/random.h"

namespace phst_rules_elisp {

using bazel::tools::cpp::runfiles::Runfiles;
using google::protobuf::util::TimeUtil;

namespace {
class MissingRunfile : public std::runtime_error {
  using std::runtime_error::runtime_error;
};
}  // namespace

static std::vector<char*> Pointers(std::vector<std::string>& strings) {
  std::vector<char*> ptrs;
  for (auto& s : strings) {
    if (s.empty()) {
      throw std::invalid_argument("empty string");
    }
    if (s.find('\0') != s.npos) {
      throw std::invalid_argument(s + " contains null character");
    }
    ptrs.push_back(&s.front());
  }
  ptrs.push_back(nullptr);
  return ptrs;
}

static std::map<std::string, std::string> CopyEnv(const char* const* envp) {
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

static std::unique_ptr<Runfiles> CreateRunfiles(const std::string& argv0) {
  std::string error;
  std::unique_ptr<Runfiles> runfiles(Runfiles::Create(argv0, &error));
  if (runfiles == nullptr) {
    throw std::runtime_error("couldn’t create runfiles: " + error);
  }
  return runfiles;
}

static std::unique_ptr<Runfiles> CreateRunfilesForTest() {
  std::string error;
  std::unique_ptr<Runfiles> runfiles(Runfiles::CreateForTest(&error));
  if (runfiles == nullptr) {
    throw std::runtime_error("couldn’t create runfiles for test: " + error);
  }
  return runfiles;
}

static std::string GetSharedDir(const std::string& install) {
  const auto emacs = JoinPath(install, "share", "emacs");
  Directory dir(emacs);
  std::set<std::string> dirs;
  for (const auto& entry : dir) {
    if (std::regex_match(entry, std::regex("[0-9][.0-9]*"))) {
      dirs.insert(entry);
    }
  }
  if (dirs.empty()) throw MissingRunfile("no shared directory found");
  if (dirs.size() != 1) {
    throw std::runtime_error("expected exactly one shared directory, got [" +
                             absl::StrJoin(dirs, ", ") + ']');
  }
  return JoinPath(emacs, *dirs.begin());
}

static absl::optional<TempStream> AddManifest(const Mode mode,
                                              std::vector<std::string>& args,
                                              Random& random) {
  if (mode == Mode::kDirect) return absl::nullopt;
  TempStream stream(TempDir(), "manifest-*.json", random);
  args.push_back("--manifest=" + stream.path());
  args.push_back("--");
  return stream;
}

static void AddToManifest(
    const std::vector<std::string>& files,
    google::protobuf::RepeatedPtrField<std::string>& field) {
  for (const auto& file : files) {
    if (IsAbsolute(file)) {
      throw std::invalid_argument("filename " + file + " is absolute");
    }
    *field.Add() = file;
  }
}

static void WriteManifest(const std::vector<std::string>& load_path,
                          const std::vector<std::string>& load_files,
                          const std::vector<std::string>& data_files,
                          const std::vector<std::string>& output_files,
                          std::ostream& file) {
  Manifest manifest;
  AddToManifest(load_path, *manifest.mutable_load_path());
  AddToManifest(load_files, *manifest.mutable_input_files());
  AddToManifest(data_files, *manifest.mutable_input_files());
  AddToManifest(output_files, *manifest.mutable_output_files());
  std::string json;
  const auto status =
      google::protobuf::util::MessageToJsonString(manifest, &json);
  if (!status.ok()) throw std::runtime_error(status.ToString());
  if (json.size() > UnsignedMax<std::streamsize>()) {
    throw std::length_error("JSON object is too big");
  }
  file.write(json.data(), static_cast<std::streamsize>(json.size()));
  file.flush();
}

static double float_seconds(
    const google::protobuf::Duration& duration) noexcept {
  return static_cast<double>(duration.seconds()) +
         static_cast<double>(duration.nanos()) / 1e9;
}

static void ConvertReport(std::istream& json_file, const std::string& xml_file) {
  using iterator = std::istreambuf_iterator<char>;
  const std::string json(iterator(json_file), iterator{});
  TestReport report;
  const auto status =
      google::protobuf::util::JsonStringToMessage(json, &report);
  if (!status.ok()) {
    throw std::runtime_error("invalid JSON report: " + json + "; " +
                             status.ToString());
  }
  File stream(xml_file,
              FileMode::kWrite | FileMode::kCreate | FileMode::kExclusive);
  const auto c_file = stream.OpenCFile("wb");
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
  stream.Close();
}

Executor::Executor(int argc, const char* const* argv, const char* const* envp)
    : orig_args_(argv, argv + argc),
      orig_env_(CopyEnv(envp)),
      runfiles_(CreateRunfiles(orig_args_.at(0))) {}

Executor::Executor(ForTest, int argc, const char* const* argv,
                   const char* const* envp)
    : orig_args_(argv, argv + argc),
      orig_env_(CopyEnv(envp)),
      runfiles_(CreateRunfilesForTest()) {}

int Executor::RunEmacs(const char* install_rel) {
  const auto install = this->Runfile(install_rel);
  const auto emacs = JoinPath(install, "bin", "emacs");
  const auto shared = GetSharedDir(install);
  const auto etc = JoinPath(shared, "etc");
  std::map<std::string, std::string> map;
  map.emplace("EMACSDATA", etc);
  map.emplace("EMACSDOC", etc);
  map.emplace("EMACSLOADPATH", JoinPath(shared, "lisp"));
  map.emplace("EMACSPATH", JoinPath(install, "libexec"));
  return this->Run(emacs, {}, map);
}

int Executor::RunBinary(const char* const wrapper, const Mode mode,
                        const std::vector<std::string>& load_path,
                        const std::vector<std::string>& load_files,
                        const std::vector<std::string>& data_files) {
  const auto emacs = this->Runfile(wrapper);
  std::vector<std::string> args;
  auto manifest = AddManifest(mode, args, random_);
  args.push_back("--quick");
  args.push_back("--batch");
  this->AddLoadPath(args, load_path);
  for (const auto& file : load_files) {
    args.push_back("--load=" + this->Runfile(file));
  }
  if (manifest) {
    WriteManifest(load_path, load_files, data_files, {}, manifest.value());
  }
  const auto code = this->Run(emacs, args, {});
  if (manifest) manifest->Close();
  return code;
}

int Executor::RunTest(const char* const wrapper, const Mode mode,
                      const std::vector<std::string>& load_path,
                      const std::vector<std::string>& srcs,
                      const std::vector<std::string>& data_files) {
  const auto emacs = this->Runfile(wrapper);
  std::vector<std::string> args;
  auto manifest = AddManifest(mode, args, random_);
  args.push_back("--quick");
  args.push_back("--batch");
  this->AddLoadPath(args, load_path);
  const auto runner = this->Runfile("phst_rules_elisp/elisp/ert/runner.elc");
  args.push_back("--load=" + runner);
  args.push_back("--funcall=elisp/ert/run-batch-and-exit");
  const auto xml_output_file = this->EnvVar("XML_OUTPUT_FILE");
  absl::optional<TempStream> report_file;
  if (!xml_output_file.empty()) {
    const std::string temp_dir = this->EnvVar("TEST_TMPDIR");
    report_file.emplace(temp_dir, "test-report-*.json", random_);
    args.push_back("--report=/:" + report_file->path());
  }
  for (const auto& file : srcs) {
    args.push_back(this->Runfile(file));
  }
  if (manifest) {
    std::vector<std::string> outputs;
    if (report_file) {
      outputs.push_back(report_file->path());
    }
    if (this->EnvVar("COVERAGE") == "1") {
      const auto coverage_dir = this->EnvVar("COVERAGE_DIR");
      if (!coverage_dir.empty()) {
        outputs.push_back(JoinPath(coverage_dir, "emacs-lisp.dat"));
      }
    }
    WriteManifest(load_path, srcs, data_files, outputs, manifest.value());
  }
  const int code = this->Run(emacs, args, {});
  if (report_file) {
    ConvertReport(*report_file, xml_output_file);
    report_file->Close();
  }
  if (manifest) manifest->Close();
  return code;
}

std::string Executor::Runfile(const std::string& rel) const {
  const std::string str = runfiles_->Rlocation(rel);
  if (str.empty()) {
    throw MissingRunfile("runfile not found: " + rel);
  }
  // Note: Don’t canonicalize the filename here, because the Python stub looks
  // for the runfiles directory in the original filename.
  return MakeAbsolute(str);
}

std::string Executor::EnvVar(const std::string& name) const noexcept {
  const auto it = orig_env_.find(name);
  return it == orig_env_.end() ? std::string() : it->second;
}

void Executor::AddLoadPath(std::vector<std::string>& args,
                           const std::vector<std::string>& load_path) const {
  constexpr const char* const runfiles_elc =
      "phst_rules_elisp/elisp/runfiles/runfiles.elc";
  bool runfile_handler_installed = false;
  for (const auto& dir : load_path) {
    try {
      args.push_back("--directory=" + this->Runfile(dir));
    } catch (const MissingRunfile&) {
      if (!absl::exchange(runfile_handler_installed, true)) {
        args.push_back("--load=" + this->Runfile(runfiles_elc));
        args.push_back("--funcall=elisp/runfiles/install-handler");
      }
      args.push_back("--directory=/bazel-runfile:" + dir);
    }
  }
}

int Executor::Run(const std::string& binary,
                  const std::vector<std::string>& args,
                  const std::map<std::string, std::string>& env) {
  auto final_args = this->BuildArgs(args);
  const auto argv = Pointers(final_args);
  auto final_env = this->BuildEnv(env);
  const auto envp = Pointers(final_env);
  int pid;
  const int error = posix_spawn(&pid, binary.c_str(), nullptr, nullptr,
                                argv.data(), envp.data());
  if (error != 0) {
    throw std::system_error(error, std::system_category(),
                            "posix_spawn(" + binary + ')');
  }
  int wstatus;
  const int status = waitpid(pid, &wstatus, 0);
  if (status != pid) {
    throw std::system_error(errno, std::system_category(),
                            "waitpid(" + std::to_string(pid) + ')');
  }
  return WIFEXITED(wstatus) ? WEXITSTATUS(wstatus) : 0xFF;
}

std::vector<std::string> Executor::BuildArgs(
    const std::vector<std::string>& prefix) const {
  std::vector<std::string> vec{orig_args_.at(0)};
  vec.insert(vec.end(), prefix.begin(), prefix.end());
  vec.insert(vec.end(), std::next(orig_args_.begin()), orig_args_.end());
  return vec;
}

std::vector<std::string> Executor::BuildEnv(
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
