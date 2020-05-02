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
#include <cstdlib>
#include <cstring>
#include <ios>
#include <iostream>
#include <iterator>
#include <map>
#include <memory>
#include <numeric>
#include <ostream>
#include <regex>
#include <set>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

#include <spawn.h>
#include <sys/types.h>
#include <sys/wait.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#include "absl/base/casts.h"
#include "absl/memory/memory.h"
#include "absl/status/status.h"
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
#include "internal/status.h"

namespace phst_rules_elisp {

using bazel::tools::cpp::runfiles::Runfiles;
using google::protobuf::util::TimeUtil;

static std::vector<char*> Pointers(std::vector<std::string>& strings) {
  std::vector<char*> ptrs;
  for (auto& s : strings) {
    if (s.empty()) {
      std::clog << "empty string" << std::endl;
      std::abort();
    }
    if (s.find('\0') != s.npos) {
      std::clog << s << " contains null character" << std::endl;
      std::abort();
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

static absl::Status ProtobufToAbslStatus(
    const google::protobuf::util::Status& status,
    const absl::string_view prefix = absl::string_view()) {
  if (status.ok()) return absl::OkStatus();
  std::string message(prefix);
  if (!prefix.empty()) message += ": ";
  message += status.message();
  return absl::Status(static_cast<absl::StatusCode>(status.code()), message);
}

static StatusOr<std::unique_ptr<Runfiles>> CreateRunfiles(
    const std::string& argv0) {
  std::string error;
  std::unique_ptr<Runfiles> runfiles(Runfiles::Create(argv0, &error));
  if (runfiles == nullptr) {
    return absl::FailedPreconditionError("couldn’t create runfiles: " + error);
  }
  return runfiles;
}

static StatusOr<std::unique_ptr<Runfiles>> CreateRunfilesForTest() {
  std::string error;
  std::unique_ptr<Runfiles> runfiles(Runfiles::CreateForTest(&error));
  if (runfiles == nullptr) {
    return absl::FailedPreconditionError("couldn’t create runfiles for test: " +
                                         error);
  }
  return runfiles;
}

static StatusOr<std::string> GetSharedDir(const std::string& install) {
  const auto emacs = JoinPath(install, "share", "emacs");
  ASSIGN_OR_RETURN(const auto dir, Directory::Open(emacs));
  std::set<std::string> dirs;
  for (const auto& entry : dir) {
    if (std::regex_match(entry, std::regex("[0-9][.0-9]*"))) {
      dirs.insert(entry);
    }
  }
  if (dirs.empty()) return absl::NotFoundError("no shared directory found");
  if (dirs.size() != 1) {
    return absl::FailedPreconditionError(
        "expected exactly one shared directory, got [" +
        absl::StrJoin(dirs, ", ") + ']');
  }
  return JoinPath(emacs, *dirs.begin());
}

static StatusOr<absl::optional<TempStream>> AddManifest(
    const Mode mode, std::vector<std::string>& args, Random& random) {
  using Type = absl::optional<TempStream>;
  if (mode == Mode::kDirect) return absl::implicit_cast<Type>(absl::nullopt);
  ASSIGN_OR_RETURN(auto stream,
                   TempStream::Open(TempDir(), "manifest-*.json", random));
  args.push_back("--manifest=" + stream.path());
  args.push_back("--");
  return absl::implicit_cast<Type>(std::move(stream));
}

enum class Absolute { kAllow, kForbid };

static void AddToManifest(
    const std::vector<std::string>& files,
    google::protobuf::RepeatedPtrField<std::string>& field,
    const Absolute absolute) {
  for (const auto& file : files) {
    if (absolute == Absolute::kForbid && IsAbsolute(file)) {
      std::clog << "filename " << file << " is absolute" << std::endl;
      std::abort();
    }
    *field.Add() = file;
  }
}

static absl::Status WriteManifest(const std::vector<std::string>& load_path,
                                  const std::vector<std::string>& load_files,
                                  const std::vector<std::string>& data_files,
                                  const std::vector<std::string>& output_files,
                                  std::ostream& file) {
  Manifest manifest;
  manifest.set_root(RUNFILES_ROOT);
  AddToManifest(load_path, *manifest.mutable_load_path(), Absolute::kForbid);
  AddToManifest(load_files, *manifest.mutable_input_files(), Absolute::kForbid);
  AddToManifest(data_files, *manifest.mutable_input_files(), Absolute::kForbid);
  AddToManifest(output_files, *manifest.mutable_output_files(),
                Absolute::kAllow);
  std::string json;
  RETURN_IF_ERROR(ProtobufToAbslStatus(
      google::protobuf::util::MessageToJsonString(manifest, &json)));
  if (json.size() > UnsignedMax<std::streamsize>()) {
    return absl::OutOfRangeError("JSON object is too big");
  }
  file.write(json.data(), static_cast<std::streamsize>(json.size()));
  RETURN_IF_ERROR(StreamStatus(file));
  file.flush();
  return StreamStatus(file);
}

static double float_seconds(
    const google::protobuf::Duration& duration) noexcept {
  return static_cast<double>(duration.seconds()) +
         static_cast<double>(duration.nanos()) / 1e9;
}

static absl::Status ConvertReport(std::istream& json_file,
                                  const std::string& xml_file) {
  using iterator = std::istreambuf_iterator<char>;
  const std::string json(iterator(json_file), iterator{});
  RETURN_IF_ERROR(StreamStatus(json_file));
  TestReport report;
  RETURN_IF_ERROR(ProtobufToAbslStatus(
      google::protobuf::util::JsonStringToMessage(json, &report),
      "invalid JSON report: " + json));
  ASSIGN_OR_RETURN(auto stream,
                   File::Open(xml_file, FileMode::kWrite | FileMode::kCreate |
                                            FileMode::kExclusive));
  ASSIGN_OR_RETURN(const auto c_file, stream.OpenCFile("wb"));
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
    return ErrorStatus(std::error_code(errno, std::generic_category()),
                       "fflush");
  }
  return stream.Close();
}

namespace {

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
  explicit Executor(int argc, const char* const* argv, const char* const* envp,
                    std::unique_ptr<Runfiles> runfiles);

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
  std::unique_ptr<Runfiles> runfiles_;
  Random random_;
};

StatusOr<Executor> Executor::Create(int argc, const char* const* argv,
                                    const char* const* envp) {
  ASSIGN_OR_RETURN(auto runfiles, CreateRunfiles(argv[0]));
  return Executor(argc, argv, envp, std::move(runfiles));
}

StatusOr<Executor> Executor::CreateForTest(int argc, const char* const* argv,
                                           const char* const* envp) {
  ASSIGN_OR_RETURN(auto runfiles, CreateRunfilesForTest());
  return Executor(argc, argv, envp, std::move(runfiles));
}

Executor::Executor(const int argc, const char* const* argv,
                   const char* const* envp, std::unique_ptr<Runfiles> runfiles)
    : orig_args_(argv, argv + argc),
      orig_env_(CopyEnv(envp)),
      runfiles_(std::move(runfiles)) {}

StatusOr<int> Executor::RunEmacs(const char* install_rel) {
  ASSIGN_OR_RETURN(const auto install, this->Runfile(install_rel));
  const auto emacs = JoinPath(install, "bin", "emacs");
  ASSIGN_OR_RETURN(const auto shared, GetSharedDir(install));
  const auto etc = JoinPath(shared, "etc");
  std::map<std::string, std::string> map;
  map.emplace("EMACSDATA", etc);
  map.emplace("EMACSDOC", etc);
  map.emplace("EMACSLOADPATH", JoinPath(shared, "lisp"));
  map.emplace("EMACSPATH", JoinPath(install, "libexec"));
  return this->Run(emacs, {}, map);
}

StatusOr<int> Executor::RunBinary(const char* const wrapper, const Mode mode,
                                  const std::vector<std::string>& load_path,
                                  const std::vector<std::string>& load_files,
                                  const std::vector<std::string>& data_files) {
  ASSIGN_OR_RETURN(const auto emacs, this->Runfile(wrapper));
  std::vector<std::string> args;
  ASSIGN_OR_RETURN(auto manifest, AddManifest(mode, args, random_));
  args.push_back("--quick");
  args.push_back("--batch");
  RETURN_IF_ERROR(this->AddLoadPath(args, load_path));
  for (const auto& file : load_files) {
    ASSIGN_OR_RETURN(const auto abs, this->Runfile(file));
    args.push_back("--load=" + file);
  }
  if (manifest) {
    RETURN_IF_ERROR(
        WriteManifest(load_path, load_files, data_files, {}, manifest.value()));
  }
  ASSIGN_OR_RETURN(const auto code, this->Run(emacs, args, {}));
  if (manifest) RETURN_IF_ERROR(manifest->Close());
  return code;
}

StatusOr<int> Executor::RunTest(const char* const wrapper, const Mode mode,
                                const std::vector<std::string>& load_path,
                                const std::vector<std::string>& srcs,
                                const std::vector<std::string>& data_files) {
  ASSIGN_OR_RETURN(const auto emacs, this->Runfile(wrapper));
  std::vector<std::string> args;
  ASSIGN_OR_RETURN(auto manifest, AddManifest(mode, args, random_));
  args.push_back("--quick");
  args.push_back("--batch");
  RETURN_IF_ERROR(this->AddLoadPath(args, load_path));
  ASSIGN_OR_RETURN(const auto runner,
                   this->Runfile("phst_rules_elisp/elisp/ert/runner.elc"));
  args.push_back("--load=" + runner);
  args.push_back("--funcall=elisp/ert/run-batch-and-exit");
  const auto xml_output_file = this->EnvVar("XML_OUTPUT_FILE");
  absl::optional<TempStream> report_file;
  if (!xml_output_file.empty()) {
    const std::string temp_dir = this->EnvVar("TEST_TMPDIR");
    ASSIGN_OR_RETURN(report_file,
                     TempStream::Open(temp_dir, "test-report-*.json", random_));
    args.push_back("--report=/:" + report_file->path());
  }
  for (const auto& file : srcs) {
    ASSIGN_OR_RETURN(const auto abs, this->Runfile(file));
    args.push_back(abs);
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
    RETURN_IF_ERROR(
        WriteManifest(load_path, srcs, data_files, outputs, manifest.value()));
  }
  ASSIGN_OR_RETURN(const auto code, this->Run(emacs, args, {}));
  if (report_file) {
    auto status = ConvertReport(*report_file, xml_output_file);
    status.Update(report_file->Close());
    RETURN_IF_ERROR(status);
  }
  if (manifest) RETURN_IF_ERROR(manifest->Close());
  return code;
}

StatusOr<std::string> Executor::Runfile(const std::string& rel) const {
  const std::string str = runfiles_->Rlocation(rel);
  if (str.empty()) return absl::NotFoundError("runfile not found: " + rel);
  // Note: Don’t canonicalize the filename here, because the Python stub looks
  // for the runfiles directory in the original filename.
  return MakeAbsolute(str);
}

std::string Executor::EnvVar(const std::string& name) const noexcept {
  const auto it = orig_env_.find(name);
  return it == orig_env_.end() ? std::string() : it->second;
}

absl::Status Executor::AddLoadPath(
    std::vector<std::string>& args,
    const std::vector<std::string>& load_path) const {
  constexpr const char* const runfiles_elc =
      "phst_rules_elisp/elisp/runfiles/runfiles.elc";
  bool runfile_handler_installed = false;
  for (const auto& dir : load_path) {
    const auto status_or_dir = this->Runfile(dir);
    if (status_or_dir.ok()) {
      args.push_back("--directory=" + status_or_dir.value());
    } else if (absl::IsNotFound(status_or_dir.status())) {
      if (!absl::exchange(runfile_handler_installed, true)) {
        ASSIGN_OR_RETURN(const auto file, this->Runfile(runfiles_elc));
        args.push_back("--load=" + file);
        args.push_back("--funcall=elisp/runfiles/install-handler");
      }
      args.push_back("--directory=/bazel-runfile:" + dir);
    } else {
      return status_or_dir.status();
    }
  }
  return absl::OkStatus();
}

StatusOr<int> Executor::Run(const std::string& binary,
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
    return ErrorStatus(std::error_code(error, std::system_category()),
                       "posix_spawn", binary);
  }
  int wstatus;
  const int status = waitpid(pid, &wstatus, 0);
  if (status != pid) return ErrnoStatus("waitpid", pid);
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

}  // namespace

int RunEmacs(const char* const install_rel, const int argc,
             const char* const* const argv, const char* const* const envp) {
  auto status_or_executor =
      phst_rules_elisp::Executor::Create(argc, argv, envp);
  if (!status_or_executor.ok()) {
    std::clog << status_or_executor.status() << std::endl;
    return EXIT_FAILURE;
  }
  auto& executor = status_or_executor.value();
  const auto status_or_code = executor.RunEmacs(install_rel);
  if (!status_or_code.ok()) {
    std::clog << status_or_executor.status() << std::endl;
    return EXIT_FAILURE;
  }
  return status_or_code.value();
}

int RunBinary(const char* const wrapper, const Mode mode,
              const std::vector<std::string>& load_path,
              const std::vector<std::string>& load_files,
              const std::vector<std::string>& data_files, const int argc,
              const char* const* const argv, const char* const* const envp) {
  auto status_or_executor =
      phst_rules_elisp::Executor::Create(argc, argv, envp);
  if (!status_or_executor.ok()) {
    std::clog << status_or_executor.status() << std::endl;
    return EXIT_FAILURE;
  }
  auto& executor = status_or_executor.value();
  const auto status_or_code =
      executor.RunBinary(wrapper, mode, load_path, load_files, data_files);
  if (!status_or_code.ok()) {
    std::clog << status_or_executor.status() << std::endl;
    return EXIT_FAILURE;
  }
  return status_or_code.value();
}

int RunTest(const char* const wrapper, const Mode mode,
            const std::vector<std::string>& load_path,
            const std::vector<std::string>& srcs,
            const std::vector<std::string>& data_files, const int argc,
            const char* const* const argv, const char* const* const envp) {
  auto status_or_executor =
      phst_rules_elisp::Executor::CreateForTest(argc, argv, envp);
  if (!status_or_executor.ok()) {
    std::clog << status_or_executor.status() << std::endl;
    return EXIT_FAILURE;
  }
  auto& executor = status_or_executor.value();
  const auto status_or_code =
      executor.RunTest(wrapper, mode, load_path, srcs, data_files);
  if (!status_or_code.ok()) {
    std::clog << status_or_executor.status() << std::endl;
    return EXIT_FAILURE;
  }
  return status_or_code.value();
}

}  // namespace phst_rules_elisp
