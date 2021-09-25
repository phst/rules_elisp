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

#include "elisp/emacs.h"

#include <cstdlib>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#pragma GCC diagnostic ignored "-Woverflow"
#include "absl/container/flat_hash_set.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#pragma GCC diagnostic pop

#include "elisp/file.h"
#include "elisp/process.h"
#include "elisp/status.h"

namespace phst_rules_elisp {

static absl::StatusOr<std::string> GetSharedDir(const std::string& install) {
  return GlobUnique(JoinPath(install, "share", "emacs", "[0-9]*"));
}

static absl::StatusOr<std::string> FindDumpFile(const std::string& libexec) {
  return GlobUnique(JoinPath(libexec, "emacs", "*", "*", "emacs.pdmp"));
}

static absl::StatusOr<int> RunEmacsImpl(const EmacsOptions& opts) {
  const auto orig_env = CopyEnv();
  ASSIGN_OR_RETURN(const auto runfiles, CreateRunfiles(opts.argv.at(0)));
  ASSIGN_OR_RETURN(const auto install, Runfile(*runfiles, opts.install_rel));
  const auto emacs = JoinPath(install, "bin", "emacs");
  ASSIGN_OR_RETURN(const auto shared, GetSharedDir(install));
  const auto etc = JoinPath(shared, "etc");
  const auto libexec = JoinPath(install, "libexec");
  std::vector<std::string> args;
  switch (opts.dump_mode) {
    case DumpMode::kPortable: {
      ASSIGN_OR_RETURN(const auto dump, FindDumpFile(libexec));
      args.push_back(absl::StrCat("--dump-file=", dump));
      break;
    }
    case DumpMode::kUnexec:
      break;
  }
  Environment map;
  map.emplace("EMACSDATA", etc);
  map.emplace("EMACSDOC", etc);
  map.emplace("EMACSLOADPATH", JoinPath(shared, "lisp"));
  map.emplace("EMACSPATH", libexec);
  return Run(opts, orig_env, *runfiles, emacs, args, map);
}

int RunEmacs(const EmacsOptions& opts) {
  const auto status_or_code = RunEmacsImpl(opts);
  if (!status_or_code.ok()) {
    std::clog << status_or_code.status() << std::endl;
    return EXIT_FAILURE;
  }
  return status_or_code.value();
}

}  // namespace phst_rules_elisp
