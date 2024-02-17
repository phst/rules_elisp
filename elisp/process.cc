// Copyright 2020, 2021, 2022, 2023, 2024 Google LLC
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

#ifdef _WIN32
#  ifndef UNICODE
#    define UNICODE
#  endif
#  ifndef _UNICODE
#    define _UNICODE
#  endif
#  ifndef STRICT
#    define STRICT
#  endif
#  ifndef NOMINMAX
#    define NOMINMAX
#  endif
#  ifndef WIN32_LEAN_AND_MEAN
#    define WIN32_LEAN_AND_MEAN
#  endif
#  include <windows.h>
#else
#  include <spawn.h>
#  include <sys/wait.h>
#  include <unistd.h>
#endif

#include <algorithm>
#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <limits>
#include <memory>
#include <string>
#include <string_view>
#include <system_error>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#ifdef __GNUC__
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wpedantic"
#  pragma GCC diagnostic ignored "-Wconversion"
#  pragma GCC diagnostic ignored "-Wsign-conversion"
#  pragma GCC diagnostic ignored "-Woverflow"
#endif
#ifdef _MSC_VER
#  pragma warning(push, 3)
#endif
#include "absl/algorithm/container.h"
#include "absl/base/attributes.h"
#include "absl/base/nullability.h"
#include "absl/cleanup/cleanup.h"  // IWYU pragma: keep
#include "absl/log/check.h"
#include "absl/log/log.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/ascii.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "absl/types/span.h"
#include "tools/cpp/runfiles/runfiles.h"
#ifdef __GNUC__
#  pragma GCC diagnostic pop
#endif
#ifdef _MSC_VER
#  pragma warning(pop)
#endif

#ifdef __APPLE__
#  include <crt_externs.h>  // for _NSGetEnviron
#endif

#include "elisp/platform.h"

namespace rules_elisp {

#ifdef _WIN32
// Build a command line that follows the Windows conventions.  See
// https://docs.microsoft.com/en-us/cpp/cpp/main-function-command-line-args?view=msvc-170#parsing-c-command-line-arguments
// and
// https://docs.microsoft.com/en-us/windows/win32/api/shellapi/nf-shellapi-commandlinetoargvw.
static std::wstring BuildCommandLine(
    const absl::Span<const std::wstring> args) {
  std::wstring result;
  bool first = true;
  for (const std::wstring& arg : args) {
    if (!std::exchange(first, false)) {
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
    const absl::Span<const std::wstring> vars) {
  std::wstring result;
  for (const std::wstring& var : vars) {
    if (var.find(L'\0') != var.npos) {
      std::wcerr << L"Environment variable " << var
                 << L" contains a null character" << std::endl;
      LOG(FATAL) << "Environment variable contains a null character";
    }
    result.append(var);
    result.push_back(L'\0');
  }
  result.push_back(L'\0');
  return result;
}
static absl::Nonnull<wchar_t*> Pointer(
    std::wstring& string ABSL_ATTRIBUTE_LIFETIME_BOUND) {
  if (string.find(L'\0') != string.npos) {
    std::wcerr << string << L" contains null character" << std::endl;
    LOG(FATAL) << "string contains null character";
  }
  return string.data();
}
#else
static std::vector<absl::Nonnull<char*>> Pointers(
    std::vector<std::string>& strings ABSL_ATTRIBUTE_LIFETIME_BOUND) {
  std::vector<absl::Nonnull<char*>> ptrs;
  for (std::string& s : strings) {
    CHECK(s.find('\0') == s.npos) << s << " contains null character";
    ptrs.push_back(s.data());
  }
  ptrs.push_back(nullptr);
  return ptrs;
}
#endif

static absl::StatusOr<Environment> CopyEnv() {
  Environment map;
#ifdef _WIN32
  struct Free {
    void operator()(const absl::Nonnull<wchar_t*> p) const noexcept {
      ::FreeEnvironmentStrings(p);
    }
  };
  const absl::Nullable<std::unique_ptr<wchar_t, Free>> envp(
      ::GetEnvironmentStringsW());
  if (envp == nullptr) {
    return absl::ResourceExhaustedError("GetEnvironmentStringsW failed");
  }
  absl::Nonnull<const wchar_t*> p = envp.get();
  while (*p != L'\0') {
    const std::wstring_view var = p;
    if (var.length() < 2) {
      std::wcerr << L"Invalid environment block entry " << var << std::endl;
      return absl::FailedPreconditionError("Invalid environment block entry");
    }
    // Skip over the first character to properly deal with the magic “per-drive
    // current directory” variables,
    // cf. https://devblogs.microsoft.com/oldnewthing/20100506-00/?p=14133.
    // Their names start with an equals sign.
    const std::size_t i = var.find(L'=', 1);
    if (i == var.npos) {
      std::wcerr << L"Invalid environment block entry " << var << std::endl;
      return absl::FailedPreconditionError("Invalid environment block entry");
    }
    const std::wstring_view key = var.substr(0, i);
    const auto [it, ok] = map.emplace(key, var.substr(i + 1));
    if (!ok) {
      std::wcerr << L"Duplicate environment variable " << key << std::endl;
      return absl::AlreadyExistsError("Duplicate environment variable");
    }
    p += var.length() + 1;
  }
#else
  const absl::Nonnull<const absl::Nonnull<const char*>*> envp =
#  ifdef __APPLE__
      // See environ(7) why this is necessary.
      *_NSGetEnviron()
#  else
      environ
#  endif
      ;
  for (absl::Nonnull<const absl::Nullable<const char*>*> pp = envp;
       *pp != nullptr; ++pp) {
    const std::string_view var = *pp;
    if (var.length() < 2) {
      return absl::FailedPreconditionError(
          absl::StrCat("Invalid environment block entry ", var));
    }
    const std::size_t i = var.find('=');
    if (i > 0 && i != var.npos) {
      const std::string_view key = var.substr(0, i);
      const auto [it, ok] = map.emplace(key, var.substr(i + 1));
      if (!ok) {
        return absl::AlreadyExistsError(
            absl::StrCat("Duplicate environment variable ", key));
      }
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
                                    const std::string_view function,
                                    const std::string_view args) {
  if (!code) return absl::OkStatus();
  return absl::Status(
      MapErrorCode(code),
      absl::StrCat(function, args.empty() ? args : absl::StrCat("(", args, ")"),
                   ": ", code.category().name(), "/", code.value(), ": ",
                   code.message()));
}

template <typename... Ts>
absl::Status ErrorStatus(const std::error_code& code,
                         const std::string_view function, Ts&&... args) {
  return MakeErrorStatus(code, function,
                         absl::StrJoin(std::forward_as_tuple(args...), ", "));
}

#ifdef _WIN32
static constexpr const unsigned int kMaxInt = std::numeric_limits<int>::max();

template <typename... Ts>
absl::Status WindowsStatus(const std::string_view function, Ts&&... args) {
  const DWORD error = ::GetLastError();
  if (error > kMaxInt) {
    return absl::InvalidArgumentError(
        absl::StrCat("error code ", error, " too large"));
  }
  return ErrorStatus(
      std::error_code(static_cast<int>(error), std::system_category()),
      function, std::forward<Ts>(args)...);
}
#else
template <typename... Ts>
absl::Status ErrnoStatus(const std::string_view function, Ts&&... args) {
  return ErrorStatus(std::error_code(errno, std::system_category()), function,
                     std::forward<Ts>(args)...);
}
#endif

template <typename Char>
absl::Status CheckASCII(const std::basic_string_view<Char> string) {
  const auto it = absl::c_find_if_not(string, [](const Char ch) {
    return ch >= 0 && ch <= std::numeric_limits<unsigned char>::max() &&
           absl::ascii_isascii(static_cast<unsigned char>(ch));
  });
  if (it != string.end()) {
    const auto val = static_cast<std::make_unsigned_t<Char>>(*it);
    return absl::InvalidArgumentError(
        absl::StrCat("non-ASCII character U+", absl::Hex(val, absl::kZeroPad4),
                     " in string"));
  }
  return absl::OkStatus();
}

// Convert ASCII strings between std::string and std::wstring.  This is only
// useful on Windows, where the native string type is std::wstring.  Only pure
// ASCII strings are supported so that we don’t have to deal with codepages.
// All Windows codepages should be ASCII-compatible;
// cf. https://docs.microsoft.com/en-us/windows/win32/intl/code-pages.
template <typename ToString, typename FromChar>
absl::StatusOr<ToString> ConvertASCII(
    const std::basic_string_view<FromChar> string) {
  enum { kMaxASCII = 0x7F };
  using ToChar = typename ToString::value_type;
  static_assert(std::numeric_limits<ToChar>::max() >= kMaxASCII,
                "destination character type too small");
  const absl::Status status = CheckASCII(string);
  if (!status.ok()) return status;
#ifdef _MSC_VER
#  pragma warning(push)
#  pragma warning(disable : 4244)
#endif
  return ToString(string.begin(), string.end());
#ifdef _MSC_VER
#  pragma warning(pop)
#endif
}

static absl::StatusOr<std::string> ToNarrow(const NativeStringView string) {
#ifdef _WIN32
  return ConvertASCII<std::string>(string);
#else
  return std::string(string);
#endif
}

static absl::StatusOr<NativeString> ToNative(const std::string_view string) {
#ifdef _WIN32
  return ConvertASCII<std::wstring>(string);
#else
  return std::string(string);
#endif
}

absl::StatusOr<Runfiles> Runfiles::Create(
    const std::string_view source_repository, const NativeStringView argv0) {
  if (const absl::Status status = CheckASCII(source_repository); !status.ok()) {
    return status;
  }
  const absl::StatusOr<std::string> narrow_argv0 = ToNarrow(argv0);
  if (!narrow_argv0.ok()) return narrow_argv0.status();
  std::string error;
  absl::Nullable<std::unique_ptr<Impl>> impl(
      Impl::Create(*narrow_argv0, std::string(source_repository), &error));
  if (impl == nullptr) {
    return absl::FailedPreconditionError(
        absl::StrCat("couldn’t create runfiles: ", error));
  }
  return Runfiles(std::move(impl));
}

absl::StatusOr<Runfiles> Runfiles::CreateForTest(
    const std::string_view source_repository) {
  if (const absl::Status status = CheckASCII(source_repository); !status.ok()) {
    return status;
  }
  std::string error;
  absl::Nullable<std::unique_ptr<Impl>> impl(
      Impl::CreateForTest(std::string(source_repository), &error));
  if (impl == nullptr) {
    return absl::FailedPreconditionError(
        absl::StrCat("couldn’t create runfiles for test: ", error));
  }
  return Runfiles(std::move(impl));
}

Runfiles::Runfiles(absl::Nonnull<std::unique_ptr<Impl>> impl)
    : impl_(std::move(impl)) {}

absl::StatusOr<NativeString> Runfiles::Resolve(
    const std::string_view name) const {
  if (const absl::Status status = CheckASCII(name); !status.ok()) return status;
  std::string resolved = impl_->Rlocation(std::string(name));
  if (resolved.empty()) {
    return absl::NotFoundError(absl::StrCat("runfile not found: ", name));
  }
#ifdef _WIN32
  absl::c_replace(resolved, '/', '\\');
#endif
  return ToNative(resolved);
}

absl::StatusOr<Environment> Runfiles::Environment() const {
  const auto& pairs = impl_->EnvVars();
  rules_elisp::Environment map;
  for (const auto& [narrow_key, narrow_value] : pairs) {
    const absl::StatusOr<NativeString> key = ToNative(narrow_key);
    if (!key.ok()) return key.status();
    const absl::StatusOr<NativeString> value = ToNative(narrow_value);
    if (!value.ok()) return value.status();
    const auto [it, ok] = map.try_emplace(*key, *value);
    if (!ok) {
      return absl::AlreadyExistsError(
          absl::StrCat("Duplicate runfiles environment variable ", narrow_key));
    }
  }
  return map;
}

absl::StatusOr<int> Run(const std::string_view binary,
                        const absl::Span<const NativeString> args,
                        const Runfiles& runfiles) {
  absl::StatusOr<NativeString> resolved_binary = runfiles.Resolve(binary);
  if (!resolved_binary.ok()) return resolved_binary.status();
  std::vector<NativeString> final_args{*resolved_binary};
  absl::StatusOr<Environment> map = runfiles.Environment();
  if (!map.ok()) return map.status();
  std::vector<NativeString> runfiles_args;
  // Pass the runfiles environment variables as separate arguments.  This is
  // necessary because the binary launcher unconditionally sets the runfiles
  // environment variables based on its own argv[0]; see
  // https://github.com/bazelbuild/bazel/blob/5.4.1/src/tools/launcher/launcher.cc
  // and https://github.com/bazelbuild/bazel/pull/16916.  We also can’t set
  // argv[0] to this binary because the launcher uses it to find its own binary;
  // see
  // https://github.com/bazelbuild/bazel/blob/5.4.1/src/tools/launcher/launcher_main.cc.
  // Once we drop support for Bazel 6.0 and below, we should be able to modify
  // argv[0] to make this launcher more transparent.
  for (const auto& [key, value] : *map) {
    runfiles_args.push_back(RULES_ELISP_NATIVE_LITERAL("--runfiles-env=") +
                            key + RULES_ELISP_NATIVE_LITERAL('=') + value);
  }
  final_args.insert(final_args.end(), runfiles_args.begin(),
                    runfiles_args.end());
  final_args.insert(final_args.end(), args.begin(), args.end());
  absl::StatusOr<Environment> orig_env = CopyEnv();
  if (!orig_env.ok()) return orig_env.status();
  // We don’t want the Python launcher to change the current working directory,
  // otherwise relative filenames will be all messed up.  See
  // https://github.com/bazelbuild/bazel/issues/7190.
  orig_env->erase(RULES_ELISP_NATIVE_LITERAL("RUN_UNDER_RUNFILES"));
  map->insert(orig_env->begin(), orig_env->end());
  std::vector<NativeString> final_env;
  for (const auto& [key, value] : *map) {
    final_env.push_back(key + RULES_ELISP_NATIVE_LITERAL('=') + value);
  }
#ifdef _WIN32
  std::wstring command_line = BuildCommandLine(final_args);
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
      ::CreateProcessW(Pointer(*resolved_binary), Pointer(command_line),
                       nullptr, nullptr, FALSE, CREATE_UNICODE_ENVIRONMENT,
                       envp.data(), nullptr, &startup_info, &process_info);
  if (!success) return WindowsStatus("CreateProcessW", binary);
  const auto close_handles = absl::MakeCleanup([&process_info] {
    ::CloseHandle(process_info.hProcess);
    ::CloseHandle(process_info.hThread);
  });
  const DWORD status = ::WaitForSingleObject(process_info.hProcess, INFINITE);
  if (status != WAIT_OBJECT_0) return WindowsStatus("WaitForSingleObject");
  DWORD code;
  success = ::GetExitCodeProcess(process_info.hProcess, &code);
  if (!success) return WindowsStatus("GetExitCodeProcess");
  return code <= kMaxInt ? code : 0xFF;
#else
  const std::vector<absl::Nonnull<char*>> argv = Pointers(final_args);
  const std::vector<absl::Nonnull<char*>> envp = Pointers(final_env);
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
#endif
}

}  // namespace rules_elisp
