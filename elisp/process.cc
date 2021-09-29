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

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <iterator>
#include <memory>
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
#pragma GCC diagnostic ignored "-Woverflow"
#include "absl/algorithm/container.h"
#include "absl/container/flat_hash_map.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "tools/cpp/runfiles/runfiles.h"
#pragma GCC diagnostic pop

#include "elisp/options.h"
#include "elisp/status.h"
#include "elisp/str.h"

#ifdef __APPLE__
#include <crt_externs.h>  // for _NSGetEnviron
#endif

namespace phst_rules_elisp {

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

Environment CopyEnv() {
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

absl::StatusOr<std::string> Runfile(const Runfiles& runfiles,
                                    const std::string& rel) {
  const std::string str = runfiles.Rlocation(rel);
  if (str.empty()) {
    return absl::NotFoundError(absl::StrCat("runfile not found: ", rel));
  }
  return str;
}

absl::StatusOr<int> Run(const Argv& orig_args, const Environment& orig_env,
                        const Runfiles& runfiles, const std::string& binary,
                        const std::vector<std::string>& args,
                        const Environment& env) {
  std::vector<std::string> final_args{orig_args.argv.at(0)};
  final_args.insert(final_args.end(), args.begin(), args.end());
  final_args.insert(final_args.end(), std::next(orig_args.argv.begin()),
                    orig_args.argv.end());
  const auto argv = Pointers(final_args);
  const auto& pairs = runfiles.EnvVars();
  Environment map(pairs.begin(), pairs.end());
  map.insert(env.begin(), env.end());
  map.insert(orig_env.begin(), orig_env.end());
  std::vector<std::string> final_env;
  for (const auto& p : map) {
    final_env.push_back(absl::StrCat(p.first, "=", p.second));
  }
  // Sort entries for hermeticity.
  absl::c_sort(final_env);
  const auto envp = Pointers(final_env);
  pid_t pid;
  const int error = posix_spawn(&pid, Pointer(binary), nullptr, nullptr,
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
