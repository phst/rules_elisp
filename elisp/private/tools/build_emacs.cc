// Copyright 2020-2026 Google LLC
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

#include <cstdlib>
#include <fstream>
#include <ios>
#include <iostream>
#include <optional>
#include <ostream>
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
#include "absl/status/status_macros.h"
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
  ABSL_ASSIGN_OR_RETURN(Environment env, Environment::Current());
  if constexpr (kWindows) {
    // Building Emacs requires MinGW, see nt/INSTALL.W64.  Therefore we
    // invoke commands through the MinGW shell, see
    // https://www.msys2.org/wiki/Launchers/#the-idea.
    env.Add(RULES_ELISP_NATIVE_LITERAL("MSYSTEM"),
            RULES_ELISP_NATIVE_LITERAL("MINGW64"));
    env.Add(RULES_ELISP_NATIVE_LITERAL("CHERE_INVOKING"),
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
  ABSL_ASSIGN_OR_RETURN(const int code,
                        RunProcess(program, args, env, options));
  if (code == 0) return absl::OkStatus();
  {
    std::ifstream stream(output.string(), std::ios::in | std::ios::binary);
    std::cerr << absl::StreamFormat("command %s failed, output follows:",
                                    QuoteForBash(program, args))
              << std::endl
              << stream.rdbuf() << std::endl;
  }
  std::cerr << std::endl
            << absl::StreamFormat("temporary build directory is %s", temp)
            << std::endl;
  return absl::UnavailableError(absl::StrFormat(
      "Command %s failed with code %d", QuoteForBash(program, args), code));
}

static absl::StatusOr<FileName> Join(
    const FileName& dir, const absl::Span<const NativeStringView> elts) {
  FileName result = dir;
  for (const NativeStringView elt : elts) {
    ABSL_ASSIGN_OR_RETURN(FileName child, result.Child(elt));
    result = std::move(child);
  }
  return result;
}

static absl::StatusOr<FileName> GlobUnique(
    const FileName& dir, const absl::Span<const NativeString> patterns) {
  ABSL_ASSIGN_OR_RETURN(FileName abs, dir.MakeAbsolute());
  FileName result = std::move(abs);
  for (const NativeString& pattern : patterns) {
    ABSL_ASSIGN_OR_RETURN(const std::vector<FileName> entries,
                          ListDirectory(result, pattern));
    if (entries.empty()) {
      return absl::NotFoundError(absl::StrFormat(
          "No entry matching %s in directory %s found", pattern, result));
    }
    if (const auto n = entries.size(); n > 1) {
      return absl::FailedPreconditionError(absl::StrFormat(
          "Found %d entries matching %s in directory %s", n, pattern, result));
    }
    ABSL_ASSIGN_OR_RETURN(FileName child, result.Child(entries.front()));
    result = std::move(child);
  }
  return result;
}

static absl::Status RenameResolved(const FileName& src, const FileName& dest) {
  if (FileExists(dest)) {
    return absl::AlreadyExistsError(
        absl::StrFormat("destination file %s already exists", dest));
  }
  ABSL_ASSIGN_OR_RETURN(const FileName resolved, src.Resolve());
  ABSL_RETURN_IF_ERROR(Rename(resolved, dest));
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
  ABSL_ASSIGN_OR_RETURN(FileName temp, CreateTemporaryDirectory());
  const FileName build =
      temp.Child(RULES_ELISP_NATIVE_LITERAL("build")).value();

  ABSL_RETURN_IF_ERROR(CopyFiles(source, build, srcs));

  // On Windows, let Bash search the MinGW path for Make.  On POSIX, do the
  // search ourselves.
  FileName make =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("make")).value();
  if constexpr (!kWindows) {
    ABSL_ASSIGN_OR_RETURN(FileName file, SearchPath(make));
    make = std::move(file);
  }

  // On Windows, let Bash search the MinGW path for GZip.  On POSIX, do the
  // search ourselves.
  FileName gzip =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("gzip")).value();
  if constexpr (!kWindows) {
    ABSL_ASSIGN_OR_RETURN(FileName file, SearchPath(gzip));
    gzip = std::move(file);
  }

  const FileName configure =
      build.Child(RULES_ELISP_NATIVE_LITERAL("configure")).value();
  ABSL_ASSIGN_OR_RETURN(const FileName cc_resolved, cc.Resolve());
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
      // Compress .el source files.  This isn’t really necessary, but works
      // around an apparent Bazel issue where some of the .el source files are
      // newer than the corresponding .elc files in the sandbox, causing
      // spurious “Source file […] newer than byte-compiled file; using older
      // file” warnings.  Emacs’s ‘load’ function only checks for the
      // modification time of .el files, not .el.gz files; see the logic in
      // lread.c.
      RULES_ELISP_NATIVE_LITERAL("--with-compress-install"),
      RULES_ELISP_NATIVE_LITERAL("MAKE=") + AsPosix(make),
      RULES_ELISP_NATIVE_LITERAL("GZIP_PROG=") + AsPosix(gzip),
      RULES_ELISP_NATIVE_LITERAL("CC=") + AsPosix(cc_resolved),
      RULES_ELISP_NATIVE_LITERAL("CFLAGS=") + NativeString(cflags),
      RULES_ELISP_NATIVE_LITERAL("LDFLAGS=") + NativeString(ldflags),
      // Try to work around https://bugs.gnu.org/79489 in older Emacsen.
      // FIXME: Remove this once we drop support for Emacs 30.
      RULES_ELISP_NATIVE_LITERAL(
          "ac_cv_func_posix_spawn_file_actions_addchdir=no"),
  };
  ABSL_RETURN_IF_ERROR(
      Run(temp, build, bash, configure, std::move(configure_args)));

  std::vector<NativeString> make_args = {RULES_ELISP_NATIVE_LITERAL("install")};
  ABSL_RETURN_IF_ERROR(Run(temp, build, bash, make, std::move(make_args)));

  // Build directory no longer needed, delete it.
  ABSL_RETURN_IF_ERROR(RemoveTree(temp));

  // Move files into hard-coded subdirectories so that emacs.cc has less work to
  // do.
  const NativeString exe_suffix =
      kWindows ? RULES_ELISP_NATIVE_LITERAL(".exe") : NativeString();
  const FileName emacs_from =
      Join(install, {RULES_ELISP_NATIVE_LITERAL("bin"),
                     RULES_ELISP_NATIVE_LITERAL("emacs") + exe_suffix})
          .value();
  const FileName emacs_to =
      install.Child(RULES_ELISP_NATIVE_LITERAL("emacs.exe")).value();
  ABSL_RETURN_IF_ERROR(RenameResolved(emacs_from, emacs_to));

  ABSL_ASSIGN_OR_RETURN(
      const FileName shared,
      GlobUnique(install, {RULES_ELISP_NATIVE_LITERAL("share"),
                           RULES_ELISP_NATIVE_LITERAL("emacs"),
                           RULES_ELISP_NATIVE_LITERAL("?*.?*")}));
  const FileName etc_from =
      shared.Child(RULES_ELISP_NATIVE_LITERAL("etc")).value();
  const FileName etc_to =
      install.Child(RULES_ELISP_NATIVE_LITERAL("etc")).value();
  ABSL_RETURN_IF_ERROR(RenameResolved(etc_from, etc_to));

  ABSL_ASSIGN_OR_RETURN(
      const absl::StatusOr<FileName> dump_from,
      GlobUnique(install, {RULES_ELISP_NATIVE_LITERAL("libexec"),
                           RULES_ELISP_NATIVE_LITERAL("emacs"),
                           RULES_ELISP_NATIVE_LITERAL("*"),
                           RULES_ELISP_NATIVE_LITERAL("*"),
                           RULES_ELISP_NATIVE_LITERAL("emacs*.pdmp")}));
  const FileName dump_to =
      install.Child(RULES_ELISP_NATIVE_LITERAL("emacs.pdmp")).value();
  ABSL_RETURN_IF_ERROR(RenameResolved(*dump_from, dump_to));

  const FileName lisp_from =
      shared.Child(RULES_ELISP_NATIVE_LITERAL("lisp")).value();
  const FileName lisp_to =
      install.Child(RULES_ELISP_NATIVE_LITERAL("lisp")).value();
  ABSL_RETURN_IF_ERROR(RenameResolved(lisp_from, lisp_to));

  return absl::OkStatus();
}

static absl::Status Main(const NativeStringView readme,
                         const NativeStringView install,
                         const NativeStringView bash, const NativeStringView cc,
                         const NativeStringView cflags,
                         const NativeStringView ldflags,
                         const NativeStringView module_header,
                         const NativeStringView srcs) {
  ABSL_ASSIGN_OR_RETURN(const FileName readme_file,
                        FileName::FromString(readme));
  ABSL_ASSIGN_OR_RETURN(const FileName source, readme_file.Parent());
  ABSL_ASSIGN_OR_RETURN(FileName install_dir, FileName::FromString(install));
  ABSL_ASSIGN_OR_RETURN(install_dir, install_dir.Resolve());
  ABSL_ASSIGN_OR_RETURN(const FileName srcs_file, FileName::FromString(srcs));
  ABSL_ASSIGN_OR_RETURN(const FileName bash_file, FileName::FromString(bash));
  ABSL_ASSIGN_OR_RETURN(const FileName cc_file, FileName::FromString(cc));

  ABSL_RETURN_IF_ERROR(Build(source, install_dir, srcs_file, bash_file, cc_file,
                             cflags, ldflags));

  if (!module_header.empty()) {
    // Copy emacs-module.h to the desired location.
    const FileName from =
        Join(install_dir, {RULES_ELISP_NATIVE_LITERAL("include"),
                           RULES_ELISP_NATIVE_LITERAL("emacs-module.h")})
            .value();
    ABSL_ASSIGN_OR_RETURN(const FileName to,
                          FileName::FromString(module_header));
    ABSL_RETURN_IF_ERROR(CopyFile(from, to));
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
