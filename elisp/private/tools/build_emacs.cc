// Copyright 2020-2025 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include <algorithm>
#include <cstdlib>
#include <fstream>
#include <ios>
#include <iostream>
#include <optional>
#include <ostream>
#include <regex>
#include <string>
#include <utility>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/base/log_severity.h"
#include "absl/log/check.h"
#include "absl/log/globals.h"
#include "absl/log/initialize.h"
#include "absl/log/log.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_format.h"
#include "absl/types/span.h"

#include "elisp/private/tools/copy.h"
#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/system.h"

namespace rules_elisp {

static void AppendQuoted(NativeString& out, NativeStringView arg) {
  out += RULES_ELISP_NATIVE_LITERAL('\'');
  while (!arg.empty()) {
    const NativeStringView::size_type i =
        arg.find(RULES_ELISP_NATIVE_LITERAL('\''));
    out += arg.substr(0, i);
    if (i == arg.npos) break;
    arg.remove_prefix(i + 1);
    out += RULES_ELISP_NATIVE_LITERAL(R"*('"'"')*");
  }
  out += RULES_ELISP_NATIVE_LITERAL('\'');
}

static NativeString QuoteForBash(const FileName& program,
                                 const absl::Span<const NativeString> args) {
  NativeString result;
  AppendQuoted(result, program.string());
  for (const NativeString& arg : args) {
    result += RULES_ELISP_NATIVE_LITERAL(' ');
    AppendQuoted(result, arg);
  }
  return result;
}

static NativeString AsPosix(const FileName& name) {
  NativeString string = name.string();
  absl::c_replace(string, kSeparator, RULES_ELISP_NATIVE_LITERAL('/'));
  return string;
}

static absl::Status Run(const FileName& temp, const FileName& build,
                        const FileName& bash, FileName program,
                        std::vector<NativeString> args) {
  absl::StatusOr<Environment> env = Environment::Current();
  if (!env.ok()) return env.status();
  if constexpr (kWindows) {
    // Building Emacs requires MinGW, see nt/INSTALL.W64.  Therefore we
    // invoke commands through the MinGW shell, see
    // https://www.msys2.org/wiki/Launchers/#the-idea.
    env->Add(RULES_ELISP_NATIVE_LITERAL("MSYSTEM"),
             RULES_ELISP_NATIVE_LITERAL("MINGW64"));
    env->Add(RULES_ELISP_NATIVE_LITERAL("CHERE_INVOKING"),
             RULES_ELISP_NATIVE_LITERAL("1"));
    args = {
        RULES_ELISP_NATIVE_LITERAL("-l"),
        RULES_ELISP_NATIVE_LITERAL("-c"),
        QuoteForBash(program, args),
    };
    program = bash;
  }
  const FileName output =
      temp.Child(RULES_ELISP_NATIVE_LITERAL("output.log")).value();
  ProcessOptions options;
  options.directory = build;
  options.output_file = output;
  const absl::StatusOr<int> code = RunProcess(program, args, *env, options);
  if (!code.ok()) return code.status();
  if (*code == 0) return absl::OkStatus();
  {
    std::ifstream stream(output.string(), std::ios::in | std::ios::binary);
    std::cerr << "command  " << QuoteForBash(program, args)
              << "failed, output follows:" << std::endl
              << stream.rdbuf() << std::endl;
  }
  {
    const FileName config_log =
        build.Child(RULES_ELISP_NATIVE_LITERAL("config.log")).value();
    std::ifstream stream(config_log.string(), std::ios::in | std::ios::binary);
    if (stream.is_open() && stream.good()) {
      std::cerr << "config.log follows:" << std::endl
                << stream.rdbuf() << std::endl;
    } else {
      std::cerr << "config.log not found" << std::endl;
    }
  }
  std::cerr << std::endl
            << absl::StreamFormat("temporary build directory is %v", temp)
            << std::endl;
  return absl::UnavailableError(absl::StrFormat(
      "Command %s failed with code %d", QuoteForBash(program, args), *code));
}

static absl::StatusOr<FileName> Join(
    const FileName& dir, const absl::Span<const NativeStringView> elts) {
  FileName result = dir;
  for (const NativeStringView elt : elts) {
    absl::StatusOr<FileName> child = result.Child(elt);
    if (!child.ok()) return child.status();
    result = *std::move(child);
  }
  return result;
}

static absl::StatusOr<FileName> GlobUnique(
    const FileName& dir, const absl::Span<const NativeString> patterns) {
  absl::StatusOr<FileName> abs = dir.MakeAbsolute();
  if (!abs.ok()) return abs.status();
  FileName result = *std::move(abs);
  for (const NativeString& pattern : patterns) {
    const std::basic_regex<NativeChar> regex(pattern);
    absl::StatusOr<std::vector<FileName>> entries = ListDirectory(result);
    if (!entries.ok()) return entries.status();
    entries->erase(std::remove_if(entries->begin(), entries->end(),
                                  [&regex](const FileName& name) {
                                    return !std::regex_match(name.string(),
                                                             regex);
                                  }),
                   entries->end());
    if (entries->empty()) {
      return absl::NotFoundError(absl::StrFormat(
          "No entry matching %s in directory %v found", pattern, result));
    }
    if (const auto n = entries->size(); n > 1) {
      return absl::FailedPreconditionError(absl::StrFormat(
          "Found %d entries matching %s in directory %v", n, pattern, result));
    }
    absl::StatusOr<FileName> child = result.Child(entries->front());
    if (!child.ok()) return child.status();
    result = *std::move(child);
  }
  return result;
}

static absl::Status RenameResolved(const FileName& src, const FileName& dest) {
  if (FileExists(dest)) {
    return absl::AlreadyExistsError(
        absl::StrFormat("destination file %v already exists", dest));
  }
  const absl::StatusOr<FileName> resolved = src.Resolve();
  if (!resolved.ok()) return resolved.status();
  if (const absl::Status status = Rename(*resolved, dest); !status.ok()) {
    return status;
  }
  if (const absl::Status status = Unlink(src);
      !status.ok() && !absl::IsNotFound(status)) {
    return status;
  }
  return absl::OkStatus();
}

static absl::Status Build(const FileName& source, const FileName& install,
                          const FileName& srcs,
                          [[maybe_unused]] const FileName& bash,
                          const FileName& cc, const NativeStringView cflags,
                          const NativeStringView ldflags) {
  absl::StatusOr<FileName> temp = CreateTemporaryDirectory();
  if (!temp.ok()) return temp.status();

  const FileName build =
      temp->Child(RULES_ELISP_NATIVE_LITERAL("build")).value();

  if (const absl::Status status = CopyFiles(source, build, srcs);
      !status.ok()) {
    return status;
  }

  // On Windows, let Bash search the MinGW path for Make.  On POSIX, do the
  // search ourselves.
  FileName make =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("make")).value();
  if constexpr (!kWindows) {
    absl::StatusOr<FileName> file = SearchPath(make);
    if (!file.ok()) return file.status();
    make = *std::move(file);
  }

  const FileName configure =
      build.Child(RULES_ELISP_NATIVE_LITERAL("configure")).value();
  const absl::StatusOr<FileName> cc_resolved = cc.Resolve();
  if (!cc_resolved.ok()) return cc_resolved.status();
  std::vector<NativeString> configure_args = {
      RULES_ELISP_NATIVE_LITERAL("--prefix=") + AsPosix(install),
      RULES_ELISP_NATIVE_LITERAL("--without-all"),
      RULES_ELISP_NATIVE_LITERAL("--without-ns"),
      RULES_ELISP_NATIVE_LITERAL("--without-x"),
      RULES_ELISP_NATIVE_LITERAL("--with-x-toolkit=no"),
      RULES_ELISP_NATIVE_LITERAL("--without-libgmp"),
      // Enable toolkit scrollbars to work around
      // https://debbugs.gnu.org/37042.
      RULES_ELISP_NATIVE_LITERAL("--with-modules"),
      RULES_ELISP_NATIVE_LITERAL("--with-toolkit-scroll-bars"),
      RULES_ELISP_NATIVE_LITERAL("--disable-build-details"),
      RULES_ELISP_NATIVE_LITERAL("MAKE=") + AsPosix(make),
      RULES_ELISP_NATIVE_LITERAL("CC=") + AsPosix(*cc_resolved),
      RULES_ELISP_NATIVE_LITERAL("CFLAGS=") + NativeString(cflags),
      RULES_ELISP_NATIVE_LITERAL("LDFLAGS=") + NativeString(ldflags),
  };
  if (const absl::Status status =
          Run(*temp, build, bash, configure, std::move(configure_args));
      !status.ok()) {
    return status;
  }

  std::vector<NativeString> make_args = {
      RULES_ELISP_NATIVE_LITERAL("install"),
      // Work around https://bugs.gnu.org/76441.
      // We can remove the workaround once we drop support for Emacs 29.
      RULES_ELISP_NATIVE_LITERAL("MAKEINFO=:"),
  };
  if (const absl::Status status =
          Run(*temp, build, bash, make, std::move(make_args));
      !status.ok()) {
    return status;
  }

  // Build directory no longer needed, delete it.
  if (const absl::Status status = RemoveTree(*temp); !status.ok()) {
    return status;
  }

  // Move files into hard-coded subdirectories so that run_emacs.py has less
  // work to do.
  const NativeString exe_suffix =
      kWindows ? RULES_ELISP_NATIVE_LITERAL(".exe") : NativeString();
  const FileName emacs_from =
      Join(install, {RULES_ELISP_NATIVE_LITERAL("bin"),
                     RULES_ELISP_NATIVE_LITERAL("emacs") + exe_suffix})
          .value();
  const FileName emacs_to =
      install.Child(RULES_ELISP_NATIVE_LITERAL("emacs.exe")).value();
  if (const absl::Status status = RenameResolved(emacs_from, emacs_to);
      !status.ok()) {
    return status;
  }

  const absl::StatusOr<FileName> shared =
      GlobUnique(install, {RULES_ELISP_NATIVE_LITERAL("share"),
                           RULES_ELISP_NATIVE_LITERAL("emacs"),
                           RULES_ELISP_NATIVE_LITERAL(R"(\d.+)")});
  if (!shared.ok()) return shared.status();
  const FileName etc_from =
      shared->Child(RULES_ELISP_NATIVE_LITERAL("etc")).value();
  const FileName etc_to =
      install.Child(RULES_ELISP_NATIVE_LITERAL("etc")).value();
  if (const absl::Status status = RenameResolved(etc_from, etc_to);
      !status.ok()) {
    return status;
  }

  const absl::StatusOr<FileName> dump_from =
      GlobUnique(install, {RULES_ELISP_NATIVE_LITERAL("libexec"),
                           RULES_ELISP_NATIVE_LITERAL("emacs"),
                           RULES_ELISP_NATIVE_LITERAL(R"(.+)"),
                           RULES_ELISP_NATIVE_LITERAL(R"(.+)"),
                           RULES_ELISP_NATIVE_LITERAL(R"(emacs.+\.pdmp)")});
  if (!dump_from.ok()) return dump_from.status();
  const FileName dump_to =
      install.Child(RULES_ELISP_NATIVE_LITERAL("emacs.pdmp")).value();
  if (const absl::Status status = RenameResolved(*dump_from, dump_to);
      !status.ok()) {
    return status;
  }

  const FileName lisp_from =
      shared->Child(RULES_ELISP_NATIVE_LITERAL("lisp")).value();
  const FileName lisp_to =
      install.Child(RULES_ELISP_NATIVE_LITERAL("lisp")).value();
  if (const absl::Status status = RenameResolved(lisp_from, lisp_to);
      !status.ok()) {
    return status;
  }

  return absl::OkStatus();
}

static absl::Status Main(const NativeStringView readme,
                         const NativeStringView install,
                         const NativeStringView bash, const NativeStringView cc,
                         const NativeStringView cflags,
                         const NativeStringView ldflags,
                         const NativeStringView module_header,
                         const NativeStringView srcs) {
  const absl::StatusOr<FileName> readme_file = FileName::FromString(readme);
  if (!readme_file.ok()) return readme_file.status();

  const absl::StatusOr<FileName> source = readme_file->Parent();
  if (!source.ok()) return source.status();

  absl::StatusOr<FileName> install_dir = FileName::FromString(install);
  if (!install_dir.ok()) return install_dir.status();
  install_dir = install_dir->Resolve();
  if (!install_dir.ok()) return install_dir.status();

  const absl::StatusOr<FileName> srcs_file = FileName::FromString(srcs);
  if (!srcs_file.ok()) return srcs_file.status();

  const absl::StatusOr<FileName> bash_file = FileName::FromString(bash);
  if (!bash_file.ok()) return bash_file.status();

  const absl::StatusOr<FileName> cc_file = FileName::FromString(cc);
  if (!cc_file.ok()) return cc_file.status();

  if (const absl::Status status = Build(*source, *install_dir, *srcs_file,
                                        *bash_file, *cc_file, cflags, ldflags);
      !status.ok()) {
    return status;
  }

  if (!module_header.empty()) {
    // Copy emacs-module.h to the desired location.
    const FileName from =
        Join(*install_dir, {RULES_ELISP_NATIVE_LITERAL("include"),
                            RULES_ELISP_NATIVE_LITERAL("emacs-module.h")})
            .value();
    const absl::StatusOr<FileName> to = FileName::FromString(module_header);
    if (!to.ok()) return to.status();
    if (const absl::Status status = CopyFile(from, *to); !status.ok()) {
      return status;
    }
  }

  return absl::OkStatus();
}

}  // namespace rules_elisp

int RULES_ELISP_MAIN(const int argc, rules_elisp::NativeChar** const argv) {
  absl::InitializeLog();
  absl::SetStderrThreshold(absl::LogSeverityAtLeast::kWarning);
  QCHECK_EQ(argc, 9);
  const absl::Status status = rules_elisp::Main(
      argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8]);
  if (!status.ok()) {
    LOG(ERROR) << status;
    return EXIT_FAILURE;
  }
}
