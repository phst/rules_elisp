// Copyright 2020, 2021 Google LLC
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

#include "elisp/process.h"

#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <string>
#include <system_error>
#include <tuple>
#include <utility>
#include <vector>

#include <spawn.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#pragma GCC diagnostic ignored "-Woverflow"
#include "absl/algorithm/container.h"
#include "absl/container/flat_hash_map.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "absl/strings/string_view.h"
#include "tools/cpp/runfiles/runfiles.h"
#pragma GCC diagnostic pop

#ifdef __APPLE__
#include <crt_externs.h>  // for _NSGetEnviron
#endif

namespace phst_rules_elisp {

using Environment = absl::flat_hash_map<std::string, std::string>;

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

static Environment CopyEnv() {
  const char* const* const envp =
#ifdef __APPLE__
      // See environ(7) why this is necessary.
      *_NSGetEnviron()
#else
      environ
#endif
      ;
  Environment map;
  for (const char* const* pp = envp; *pp != nullptr; ++pp) {
    const char* p = *pp;
    const char* q = std::strchr(p, '=');
    if (q != nullptr) {
      map.emplace(std::string(p, q), std::string(q + 1));
    }
  }
  return map;
}

static absl::StatusCode MapErrorCode(const std::error_code& code) {
  const auto condition = code.default_error_condition();
  if (condition.category() != std::generic_category()) {
    return absl::StatusCode::kUnknown;
  }
  switch (static_cast<std::errc>(condition.value())) {
    case std::errc::file_exists:
      return absl::StatusCode::kAlreadyExists;
    case std::errc::function_not_supported:
      return absl::StatusCode::kUnimplemented;
    case std::errc::no_space_on_device:
      return absl::StatusCode::kResourceExhausted;
    case std::errc::no_such_file_or_directory:
      return absl::StatusCode::kNotFound;
    case std::errc::operation_canceled:
      return absl::StatusCode::kCancelled;
    case std::errc::permission_denied:
      return absl::StatusCode::kPermissionDenied;
    case std::errc::timed_out:
      return absl::StatusCode::kDeadlineExceeded;
    default:
      return absl::StatusCode::kUnknown;
  }
}

static absl::Status MakeErrorStatus(const std::error_code& code,
                                    const absl::string_view function,
                                    const absl::string_view args) {
  if (!code) return absl::OkStatus();
  return absl::Status(
      MapErrorCode(code),
      absl::StrCat(function, args.empty() ? args : absl::StrCat("(", args, ")"),
                   ": ", code.category().name(), "/", code.value(), ": ",
                   code.message()));
}

template <typename... Ts>
absl::Status ErrorStatus(const std::error_code& code,
                         const absl::string_view function, Ts&&... args) {
  return MakeErrorStatus(code, function,
                         absl::StrJoin(std::forward_as_tuple(args...), ", "));
}

template <typename... Ts>
absl::Status ErrnoStatus(const absl::string_view function, Ts&&... args) {
  return ErrorStatus(std::error_code(errno, std::system_category()), function,
                     std::forward<Ts>(args)...);
}

absl::StatusOr<std::unique_ptr<Runfiles>> CreateRunfiles(
    const std::string& argv0) {
  std::string error;
  std::unique_ptr<Runfiles> runfiles(Runfiles::Create(argv0, &error));
  if (runfiles == nullptr) {
    return absl::FailedPreconditionError(
        absl::StrCat("couldn’t create runfiles: ", error));
  }
  return std::move(runfiles);
}

absl::StatusOr<int> Run(const std::string& binary,
                        const std::vector<std::string>& args,
                        const Runfiles& runfiles) {
  const Environment orig_env = CopyEnv();
  const std::string resolved_binary = runfiles.Rlocation(binary);
  if (resolved_binary.empty()) {
    return absl::NotFoundError(absl::StrCat("runfile not found: ", binary));
  }
  std::vector<std::string> final_args{resolved_binary};
  final_args.insert(final_args.end(), args.begin(), args.end());
  const auto argv = Pointers(final_args);
  const auto& pairs = runfiles.EnvVars();
  Environment map(pairs.begin(), pairs.end());
  map.insert(orig_env.begin(), orig_env.end());
  std::vector<std::string> final_env;
  for (const auto& p : map) {
    final_env.push_back(absl::StrCat(p.first, "=", p.second));
  }
  // Sort entries for hermeticity.
  absl::c_sort(final_env);
  const auto envp = Pointers(final_env);
  pid_t pid;
  const int error = posix_spawn(&pid, argv.front(), nullptr, nullptr,
                                argv.data(), envp.data());
  if (error != 0) {
    return ErrorStatus(std::error_code(error, std::system_category()),
                       "posix_spawn", binary);
  }
  int wstatus;
  const pid_t status = waitpid(pid, &wstatus, 0);
  if (status != pid) return ErrnoStatus("waitpid", pid);
  return WIFEXITED(wstatus) ? WEXITSTATUS(wstatus) : 0xFF;
}

}  // namespace phst_rules_elisp
