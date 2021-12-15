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

#ifdef PHST_RULES_ELISP_WINDOWS
#ifndef UNICODE
#define UNICODE
#endif
#ifndef _UNICODE
#define _UNICODE
#endif
#ifndef STRICT
#define STRICT
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#else
#include <spawn.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#endif

#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <limits>
#include <memory>
#include <string>
#include <system_error>
#include <tuple>
#include <utility>
#include <vector>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#pragma GCC diagnostic ignored "-Woverflow"
#include "absl/algorithm/container.h"
#include "absl/cleanup/cleanup.h"
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

#include "elisp/platform.h"

namespace phst_rules_elisp {

#ifdef PHST_RULES_ELISP_WINDOWS
// Build a command line that follows the Windows conventions.  See
// https://docs.microsoft.com/en-us/cpp/cpp/main-function-command-line-args?view=msvc-170#parsing-c-command-line-arguments
// and
// https://docs.microsoft.com/en-us/windows/win32/api/shellapi/nf-shellapi-commandlinetoargvw.
static std::wstring BuildCommandLine(const std::vector<std::wstring>& args) {
  std::wstring result;
  bool first = true;
  for (const std::wstring& arg : args) {
    if (!absl::exchange(first, false)) {
      result.push_back(L' ');
    }
    if (!arg.empty() && arg.find_first_of(L" \t\n\v\"") == arg.npos) {
      // No quoting needed.
      result.append(arg);
    } else {
      result.push_back(L'"');
      const auto end = arg.end();
      for (auto it = arg.begin(); it != end; ++it) {
        const auto begin = it;
        // Find current run of backslashes.
        it = std::find_if(it, end, [](wchar_t c) { return c != L'\\'; });
        // In any case, we need to copy over the same number of backslashes at
        // least once.
        result.append(begin, it);
        if (it == end || *it == L'"') {
          // The current run of backslashes will be followed by a quotation
          // mark, either the terminator or a character in the argument.  We
          // have to double the backslashes in this case (first and second case
          // in
          // https://docs.microsoft.com/en-us/windows/win32/api/shellapi/nf-shellapi-commandlinetoargvw#remarks).
          result.append(begin, it);
        }
        // If we’re at the end, only insert the trailing quotation mark (first
        // case in
        // https://docs.microsoft.com/en-us/windows/win32/api/shellapi/nf-shellapi-commandlinetoargvw#remarks).
        if (it == end) break;
        // If we have a quotation mark in the argument, we have to add yet
        // another backslash (second case in
        // https://docs.microsoft.com/en-us/windows/win32/api/shellapi/nf-shellapi-commandlinetoargvw#remarks).
        if (*it == L'"') result.push_back(L'\\');
        // Add the argument character itself.  This is correct even if it’s a
        // quotation mark.
        result.push_back(*it);
      }
      result.push_back(L'"');
    }
  }
  return result;
}
// Build an environment block that CreateProcessW can use.  See
// https://docs.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-createprocessw.
static std::wstring BuildEnvironmentBlock(
    const std::vector<std::wstring>& vars) {
  std::wstring result;
  for (const std::wstring& var : vars) {
    if (var.find(L'\0') != var.npos) {
      std::wclog << L"Environment variable " << var
                 << L" contains a null character" << std::endl;
      std::abort();
    }
    result.append(var);
    result.push_back(L'\0');
  }
  result.push_back(L'\0');
  return result;
}
static wchar_t* Pointer(std::wstring& string) {
  if (string.empty()) {
    std::wclog << L"empty string" << std::endl;
    std::abort();
  }
  if (string.find('\0') != string.npos) {
    std::wclog << string << L" contains null character" << std::endl;
    std::abort();
  }
  return &string.front();
}
#else
static std::vector<char*> Pointers(std::vector<std::string>& strings) {
  std::vector<char*> ptrs;
  for (std::string& s : strings) {
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
#endif

static Environment CopyEnv() {
  Environment map;
#ifdef PHST_RULES_ELISP_WINDOWS
  struct Free {
    void operator()(wchar_t* p) const noexcept { ::FreeEnvironmentStrings(p); }
  };
  const std::unique_ptr<wchar_t, Free> envp(::GetEnvironmentStringsW());
  if (envp == nullptr) {
    // This cannot really happen in practice.
    std::wclog << L"GetEnvironmentStringsW failed" << std::endl;
    std::abort();
  }
  const wchar_t* p = envp.get();
  while (*p != L'\0') {
    // Skip over the first character to properly deal with the magic “per-drive
    // current directory” variables,
    // cf. https://devblogs.microsoft.com/oldnewthing/20100506-00/?p=14133.
    // Their names start with an equals sign.
    const wchar_t* const q = std::wcschr(p + 1, L'=');
    if (q == nullptr) {
      std::wclog << "Invalid environment block entry " << p << std::endl;
      std::abort();
    }
    map.emplace(std::wstring(p, q), std::wstring(q + 1));
    p = std::wcschr(p, L'\0');
    if (p == nullptr) {
      // This can’t happen because the environment block is terminated by a
      // double null character.
      std::wclog
          << L"GetEnvironmentStringsW returned an invalid environment block"
          << std::endl;
      std::abort();
    }
    ++p;
  }
#else
  const char* const* const envp =
#ifdef __APPLE__
      // See environ(7) why this is necessary.
      *_NSGetEnviron()
#else
      environ
#endif
      ;
  for (const char* const* pp = envp; *pp != nullptr; ++pp) {
    const char* p = *pp;
    const char* q = std::strchr(p, '=');
    if (q != nullptr) {
      map.emplace(std::string(p, q), std::string(q + 1));
    }
  }
#endif
  return map;
}

static absl::StatusCode MapErrorCode(const std::error_code& code) {
  const std::error_condition condition = code.default_error_condition();
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

#ifdef PHST_RULES_ELISP_WINDOWS
template <typename... Ts>
absl::Status WindowsStatus(const absl::string_view function, Ts&&... args) {
  const DWORD error = ::GetLastError();
  if (error > std::numeric_limits<int>::max()) {
    return absl::InvalidArgumentError(
        absl::StrCat("error code ", error, " too large"));
  }
  return ErrorStatus(
      std::error_code(static_cast<int>(error), std::system_category()),
      function, std::forward<Ts>(args)...);
}
#else
template <typename... Ts>
absl::Status ErrnoStatus(const absl::string_view function, Ts&&... args) {
  return ErrorStatus(std::error_code(errno, std::system_category()), function,
                     std::forward<Ts>(args)...);
}
#endif

enum { kMaxASCII = 0x7F };

template <typename String>
absl::Status CheckASCII(const String& string) {
  using Traits = typename String::traits_type;
  using Char = typename Traits::char_type;
  const auto it = absl::c_find_if(string, [](const Char& ch) {
    return Traits::lt(ch, 0) || Traits::lt(kMaxASCII, ch);
  });
  if (it != string.end()) {
    return absl::InvalidArgumentError(absl::StrCat(
        "non-ASCII character U+",
        absl::Hex(Traits::to_int_type(*it), absl::kZeroPad4), " in string"));
  }
  return absl::OkStatus();
}

// Convert ASCII strings between std::string and std::wstring.  This is only
// useful on Windows, where the native string type is std::wstring.  Only pure
// ASCII strings are supported so that we don’t have to deal with codepages.
// All Windows codepages should be ASCII-compatible;
// cf. https://docs.microsoft.com/en-us/windows/win32/intl/code-pages.
template <typename ToString, typename FromString>
absl::StatusOr<ToString> ConvertASCII(const FromString& string) {
  using ToChar = typename ToString::traits_type::char_type;
  static_assert(std::numeric_limits<ToChar>::max() >= kMaxASCII,
                "destination character type too small");
  const absl::Status status = CheckASCII(string);
  if (!status.ok()) return status;
  return ToString(string.begin(), string.end());
}

static absl::StatusOr<std::string> ToNarrow(const NativeString& string) {
#ifdef PHST_RULES_ELISP_WINDOWS
  return ConvertASCII<std::string>(string);
#else
  return string;
#endif
}

static absl::StatusOr<NativeString> ToNative(const std::string& string) {
#ifdef PHST_RULES_ELISP_WINDOWS
  return ConvertASCII<std::wstring>(string);
#else
  return string;
#endif
}

absl::StatusOr<Runfiles> Runfiles::Create(const NativeString& argv0) {
  const absl::StatusOr<std::string> narrow_argv0 = ToNarrow(argv0);
  if (!narrow_argv0.ok()) return narrow_argv0.status();
  std::string error;
  std::unique_ptr<Impl> impl(Impl::Create(*narrow_argv0, &error));
  if (impl == nullptr) {
    return absl::FailedPreconditionError(
        absl::StrCat("couldn’t create runfiles: ", error));
  }
  return Runfiles(std::move(impl));
}

absl::StatusOr<Runfiles> Runfiles::CreateForTest() {
  std::string error;
  std::unique_ptr<Impl> impl(Impl::CreateForTest(&error));
  if (impl == nullptr) {
    return absl::FailedPreconditionError(
        absl::StrCat("couldn’t create runfiles for test: ", error));
  }
  return Runfiles(std::move(impl));
}

Runfiles::Runfiles(std::unique_ptr<Impl> impl) : impl_(std::move(impl)) {}

absl::StatusOr<NativeString> Runfiles::Resolve(const std::string& name) const {
  const absl::Status status = CheckASCII(name);
  if (!status.ok()) return status;
  std::string resolved = impl_->Rlocation(name);
  if (resolved.empty()) {
    return absl::NotFoundError(absl::StrCat("runfile not found: ", name));
  }
#ifdef PHST_RULES_ELISP_WINDOWS
  absl::c_replace(resolved, '/', '\\');
#endif
  return ToNative(resolved);
}

absl::StatusOr<Environment> Runfiles::Environment() const {
  const auto& pairs = impl_->EnvVars();
  phst_rules_elisp::Environment map;
  for (const auto& p : pairs) {
    const absl::StatusOr<NativeString> key = ToNative(p.first);
    if (!key.ok()) return key.status();
    const absl::StatusOr<NativeString> value = ToNative(p.second);
    if (!value.ok()) return value.status();
    map.emplace(*key, *value);
  }
  return map;
}

absl::StatusOr<int> Run(std::string binary,
                        const std::vector<NativeString>& args,
                        const Runfiles& runfiles) {
#ifdef PHST_RULES_ELISP_WINDOWS
  binary += ".exe";
#endif
  absl::StatusOr<NativeString> resolved_binary = runfiles.Resolve(binary);
  if (!resolved_binary.ok()) return resolved_binary.status();
  std::vector<NativeString> final_args{*resolved_binary};
  absl::StatusOr<Environment> map = runfiles.Environment();
  if (!map.ok()) return map.status();
  std::vector<NativeString> runfiles_args;
  // Pass the runfiles environment variables as separate arguments.  This is
  // necessary because the binary launcher unconditionally sets the runfiles
  // environment variables based on its own argv[0]; see
  // https://github.com/bazelbuild/bazel/blob/6.0.0-pre.20211110.1/src/tools/launcher/launcher.cc.
  // We also can’t set argv[0] to this binary because the launcher uses it to
  // find and its own binary; see
  // https://github.com/bazelbuild/bazel/blob/6.0.0-pre.20211110.1/src/tools/launcher/launcher_main.cc.
  for (const auto& p : *map) {
    runfiles_args.push_back(PHST_RULES_ELISP_NATIVE_LITERAL("--runfiles_env=") +
                            p.first + PHST_RULES_ELISP_NATIVE_LITERAL('=') +
                            p.second);
  }
  // Sort entries for hermeticity.
  absl::c_sort(runfiles_args);
  final_args.insert(final_args.end(), runfiles_args.begin(),
                    runfiles_args.end());
  final_args.insert(final_args.end(), args.begin(), args.end());
  return Run(final_args, runfiles);
}

absl::StatusOr<int> Run(std::vector<NativeString> args,
                        const Runfiles& runfiles) {
  if (args.empty()) {
    std::cerr << "empty argument list" << std::endl;
    std::abort();
  }
  Environment orig_env = CopyEnv();
  // We don’t want the Python launcher to change the current working directory,
  // otherwise relative filenames will be all messed up.  See
  // https://github.com/bazelbuild/bazel/issues/7190.
  orig_env.erase(PHST_RULES_ELISP_NATIVE_LITERAL("RUN_UNDER_RUNFILES"));
  absl::StatusOr<Environment> map = runfiles.Environment();
  if (!map.ok()) return map.status();
  map->insert(orig_env.begin(), orig_env.end());
  std::vector<NativeString> final_env;
  for (const auto& p : *map) {
    final_env.push_back(p.first + PHST_RULES_ELISP_NATIVE_LITERAL('=') +
                        p.second);
  }
  // Sort entries for hermeticity.
  absl::c_sort(final_env);
#ifdef PHST_RULES_ELISP_WINDOWS
  std::wstring binary = args.front();
  std::wstring command_line = BuildCommandLine(args);
  std::wstring envp = BuildEnvironmentBlock(final_env);
  STARTUPINFOW startup_info;
  startup_info.cb = sizeof startup_info;
  startup_info.lpReserved = nullptr;
  startup_info.lpDesktop = nullptr;
  startup_info.lpTitle = nullptr;
  startup_info.dwFlags = 0;
  startup_info.cbReserved2 = 0;
  startup_info.lpReserved2 = nullptr;
  PROCESS_INFORMATION process_info;
  BOOL success =
      ::CreateProcessW(Pointer(binary), Pointer(command_line), nullptr, nullptr,
                       FALSE, CREATE_UNICODE_ENVIRONMENT, &envp.front(),
                       nullptr, &startup_info, &process_info);
  if (!success) {
    return WindowsStatus("CreateProcessW",
                         std::string(binary.begin(), binary.end()));
  }
  const auto close_handles = absl::MakeCleanup([&process_info] {
    ::CloseHandle(process_info.hProcess);
    ::CloseHandle(process_info.hThread);
  });
  const DWORD status = ::WaitForSingleObject(process_info.hProcess, INFINITE);
  if (status != WAIT_OBJECT_0) return WindowsStatus("WaitForSingleObject");
  DWORD code;
  success = ::GetExitCodeProcess(process_info.hProcess, &code);
  if (!success) return WindowsStatus("GetExitCodeProcess");
  return code <= std::numeric_limits<int>::max() ? code : 0xFF;
#else
  const std::vector<char*> argv = Pointers(args);
  const std::vector<char*> envp = Pointers(final_env);
  pid_t pid;
  const int error = posix_spawn(&pid, argv.front(), nullptr, nullptr,
                                argv.data(), envp.data());
  if (error != 0) {
    return ErrorStatus(std::error_code(error, std::system_category()),
                       "posix_spawn", argv.front());
  }
  int wstatus;
  const pid_t status = waitpid(pid, &wstatus, 0);
  if (status != pid) return ErrnoStatus("waitpid", pid);
  return WIFEXITED(wstatus) ? WEXITSTATUS(wstatus) : 0xFF;
#endif
}

}  // namespace phst_rules_elisp
