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

#include "elisp/private/tools/system.h"

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

#ifdef __APPLE__
#  include <crt_externs.h>  // for _NSGetEnviron
#endif

#include <cerrno>
#include <cstddef>
#include <cstdlib>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <system_error>
#include <type_traits>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/base/attributes.h"
#include "absl/base/nullability.h"
#include "absl/cleanup/cleanup.h"
#include "absl/container/hash_container_defaults.h"
#include "absl/log/check.h"
#include "absl/log/die_if_null.h"
#include "absl/log/log.h"
#include "absl/status/status.h"
#include "absl/strings/str_cat.h"

#include "elisp/private/tools/numeric.h"
#include "elisp/private/tools/strings.h"

namespace rules_elisp {

static constexpr std::size_t kMaxFilename{
#ifdef _WIN32
    MAX_PATH
#else
    PATH_MAX
#endif
};

std::size_t MaxFilename() { return kMaxFilename; }

absl::Status MakeErrorStatus(const std::error_code& code,
                             const std::string_view function,
                             const std::string_view args) {
  if (!code) return absl::OkStatus();
  const std::error_condition condition = code.default_error_condition();
  const std::string message = absl::StrCat(
      function, args.empty() ? args : absl::StrCat("(", args, ")"), ": ",
      code.category().name(), "/", code.value(), ": ", code.message());
  return condition.category() == std::generic_category()
             ? absl::ErrnoToStatus(condition.value(), message)
             : absl::UnknownError(message);
}

[[nodiscard]] std::error_code WindowsError() {
#ifdef _WIN32
  const DWORD code = ::GetLastError();
  const std::optional<int> i = CastNumberOpt<int>(code);
  return i.has_value() ? std::error_code(*i, std::system_category())
                       : std::make_error_code(std::errc::value_too_large);
#else
  return std::make_error_code(std::errc::operation_not_supported);
#endif
}

[[nodiscard]] std::error_code ErrnoError() {
  const int code = errno;
  return std::error_code(code, std::system_category());
}

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
    CHECK_EQ(var.find(L'\0'), var.npos)
        << "Environment variable " << Escape(var)
        << " contains a null character";
    result.append(var);
    result.push_back(L'\0');
  }
  result.push_back(L'\0');
  return result;
}
static wchar_t* absl_nonnull
Pointer(std::wstring& string ABSL_ATTRIBUTE_LIFETIME_BOUND) {
  CHECK_EQ(string.find(L'\0'), string.npos)
      << Escape(string) << " contains null character";
  return string.data();
}
#else
static std::vector<char* absl_nonnull> Pointers(
    std::vector<std::string>& strings ABSL_ATTRIBUTE_LIFETIME_BOUND) {
  std::vector<char* absl_nonnull> ptrs;
  for (std::string& s : strings) {
    CHECK_EQ(s.find('\0'), s.npos) << s << " contains null character";
    ptrs.push_back(s.data());
  }
  ptrs.push_back(nullptr);
  return ptrs;
}
#endif

#ifndef _WIN32
static absl::StatusOr<std::string> WorkingDirectory() {
  struct Free {
    void operator()(char* const absl_nonnull p) const { std::free(p); }
  };
  // Assume that we always run on an OS that allocates a buffer when passed a
  // null pointer.
  const absl_nullable std::unique_ptr<char, Free> ptr(getcwd(nullptr, 0));
  if (ptr == nullptr) return ErrnoStatus("getcwd", nullptr, 0);
  // See the Linux man page for getcwd(3) why this can happen.
  if (*ptr != '/') {
    return absl::NotFoundError(absl::StrCat("Current working directory ",
                                            ptr.get(), " is unreachable"));
  }
  return ptr.get();
}
#endif

absl::StatusOr<NativeString> MakeAbsolute(const NativeStringView file) {
  if (file.empty()) return absl::InvalidArgumentError("empty filename");
#ifdef _WIN32
  std::wstring string(file);
  // 0x8000 is the maximum length of a filename on Windows.  See
  // https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfullpathnamew#parameters.
  constexpr DWORD size{0x8000};
  std::wstring buffer(size, L'\0');
  const DWORD result =
      ::GetFullPathNameW(Pointer(string), size, buffer.data(), nullptr);
  if (result == 0) {
    return WindowsStatus("GetFullPathNameW", Escape(file), size, "...",
                         nullptr);
  }
  if (result > size) {
    return absl::OutOfRangeError(
        absl::StrCat("Length of absolute file name (", result,
                     ") overflows buffer of size ", size));
  }
  return buffer.substr(0, result);
#else
  if (file.front() == '/') return NativeString(file);
  const absl::StatusOr<std::string> cwd = WorkingDirectory();
  if (!cwd.ok()) return cwd.status();
  return absl::StrCat(*cwd, "/", file);
#endif
}

namespace {

class EnvironmentBlock final {
 private:
  struct Free;
#ifdef _WIN32
  struct Free {
    void operator()(wchar_t* const absl_nonnull p) const noexcept {
      const BOOL ok = ::FreeEnvironmentStringsW(p);
      // If this fails, we can’t really do much except logging the error.
      if (!ok) LOG(ERROR) << WindowsStatus("FreeEnvironmentStringsW");
    }
  };
#endif

  using Pointer = std::conditional_t<kWindows, std::unique_ptr<wchar_t, Free>,
                                     const char* const absl_nullable*>;

 public:
  static absl::StatusOr<EnvironmentBlock> Current() {
#ifdef _WIN32
    absl_nullable Pointer envp(::GetEnvironmentStringsW());
    if (envp == nullptr) {
      // Don’t use WindowsStatus since the documentation
      // (https://learn.microsoft.com/en-us/windows/win32/api/processenv/nf-processenv-getenvironmentstringsw)
      // doesn’t say we can use GetLastError.
      return absl::ResourceExhaustedError("GetEnvironmentStringsW failed");
    }
    return EnvironmentBlock(std::move(envp));
#else
#  ifdef __APPLE__
    // See environ(7) why this is necessary.
    return EnvironmentBlock(*_NSGetEnviron());
#  else
    return EnvironmentBlock(environ);
#  endif
#endif
  }

  EnvironmentBlock(EnvironmentBlock&&) = default;
  EnvironmentBlock& operator=(EnvironmentBlock&&) = default;

  struct Sentinel final {};

  class Iterator final {
   private:
    using Environ = const char* const absl_nullable* absl_nonnull;
    using Pointer =
        std::conditional_t<kWindows, const wchar_t* absl_nonnull, Environ>;

   public:
    explicit Iterator(const Pointer ptr) : cur_(ABSL_DIE_IF_NULL(ptr)) {}

    Iterator(const Iterator&) = default;
    Iterator& operator=(const Iterator&) = default;

    NativeStringView operator*() const {
      CHECK(!AtEnd());
#ifdef _WIN32
      return cur_;
#else
      return *cur_;
#endif
    }

    Iterator& operator++() {
      CHECK(!AtEnd());
#ifdef _WIN32
      cur_ = cur_.data() + cur_.length() + 1;
#else
      ++cur_;
#endif
      return *this;
    }

    bool operator!=(Sentinel) const {
      return !AtEnd();
    }

   private:
    std::conditional_t<kWindows, std::wstring_view, Environ> cur_;

    ABSL_MUST_USE_RESULT bool AtEnd() const {
#ifdef _WIN32
      return cur_.empty();
#else
      return *cur_ == nullptr;
#endif
    }
  };

  Iterator begin() const {
#ifdef _WIN32
    return Iterator(ptr_.get());
#else
    return Iterator(ptr_);
#endif
  }

  Sentinel end() const { return {}; }

 private:
  explicit EnvironmentBlock(absl_nonnull Pointer ptr)
      : ptr_(std::move(ABSL_DIE_IF_NULL(ptr))) {}

  absl_nonnull Pointer ptr_;
};

}  // namespace

absl::StatusOr<Environment> Environment::Current() {
  const absl::StatusOr<EnvironmentBlock> block = EnvironmentBlock::Current();
  if (!block.ok()) return block.status();
  Map map;
  // Skip over the first character to properly deal with the magic “per-drive
  // current directory” variables on Windows,
  // cf. https://devblogs.microsoft.com/oldnewthing/20100506-00/?p=14133.  Their
  // names start with an equals sign.
  constexpr std::size_t skip = kWindows ? 1 : 0;
  for (const NativeStringView var : *block) {
    if (var.length() < 2) {
      return absl::FailedPreconditionError(
          absl::StrCat("Invalid environment block entry ", Escape(var)));
    }
    const std::size_t i = var.find(RULES_ELISP_NATIVE_LITERAL('='), skip);
    if (i == 0 || i == var.npos) {
      return absl::FailedPreconditionError(
          absl::StrCat("Invalid environment block entry ", Escape(var)));
    }
    const NativeStringView key = var.substr(0, i);
    const auto [it, ok] = map.emplace(key, var.substr(i + 1));
    if (!ok) {
      return absl::AlreadyExistsError(
          absl::StrCat("Duplicate environment variable ", Escape(key)));
    }
  }
  return Environment(std::move(map));
}

#ifdef _WIN32
static std::wstring CanonicalizeEnvironmentVariable(
    const std::wstring_view string) {
  if (string.empty()) return {};
  constexpr LCID locale = LOCALE_INVARIANT;
  constexpr DWORD flags = LCMAP_UPPERCASE;
  const int length = CastNumberOpt<int>(string.length()).value();
  int result = ::LCMapStringW(locale, flags, string.data(), length, nullptr, 0);
  CHECK_GT(result, 0) << WindowsStatus("LCMapStringW", locale, flags, "...",
                                       length, nullptr, 0);
  std::wstring buffer(result, L'\0');
  result = ::LCMapStringW(locale, flags, string.data(), length, buffer.data(),
                          result);
  CHECK_GT(result, 0) << WindowsStatus("LCMapStringW", locale, flags, "...",
                                       length, "...", buffer.size());
  return buffer.substr(0, result);
}
#else
static std::string_view CanonicalizeEnvironmentVariable(
    const std::string_view string ABSL_ATTRIBUTE_LIFETIME_BOUND) {
  return string;
}
#endif

std::size_t Environment::Hash::operator()(const NativeStringView string) const {
  const absl::DefaultHashContainerHash<NativeString> base;
  return base(CanonicalizeEnvironmentVariable(string));
}

bool Environment::Equal::operator()(const NativeStringView a,
                                    const NativeStringView b) const {
  const absl::DefaultHashContainerEq<NativeString> base;
  return base(CanonicalizeEnvironmentVariable(a),
              CanonicalizeEnvironmentVariable(b));
}

absl::StatusOr<int> Run(std::vector<NativeString>& args,
                        const Environment& env) {
  CHECK(!args.empty());
  CHECK(!env.empty());
  std::vector<NativeString> final_env;
  for (const auto& [key, value] : env) {
    final_env.push_back(key + RULES_ELISP_NATIVE_LITERAL('=') + value);
  }
  // Sort entries for hermeticity.
  absl::c_sort(final_env);
#ifdef _WIN32
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
      ::CreateProcessW(Pointer(args.front()), Pointer(command_line), nullptr,
                       nullptr, FALSE, CREATE_UNICODE_ENVIRONMENT, envp.data(),
                       nullptr, &startup_info, &process_info);
  if (!success) {
    return WindowsStatus("CreateProcessW", Escape(args.front()),
                         Escape(command_line));
  }
  ::CloseHandle(process_info.hThread);
  const auto close_handle = absl::MakeCleanup(
      [&process_info] { ::CloseHandle(process_info.hProcess); });
  const DWORD status = ::WaitForSingleObject(process_info.hProcess, INFINITE);
  if (status != WAIT_OBJECT_0) return WindowsStatus("WaitForSingleObject");
  DWORD code;
  success = ::GetExitCodeProcess(process_info.hProcess, &code);
  if (!success) return WindowsStatus("GetExitCodeProcess");
  // Emacs returns −1 on error, which the Windows C runtime will translate to
  // 0xFFFFFFFF.  Undo this cast, assuming that both DWORD and int use two’s
  // complement representation without padding bits.
  static_assert(sizeof(int) == sizeof(DWORD));
  static_assert(std::has_unique_object_representations_v<int>);
  static_assert(std::has_unique_object_representations_v<DWORD>);
  using IntLimits = std::numeric_limits<int>;
  static_assert(IntLimits::radix == 2);
  static_assert(IntLimits::digits == 31);
  static_assert(-1 == ~0);  // https://stackoverflow.com/a/16501113
  using DWORDLimits = std::numeric_limits<DWORD>;
  static_assert(DWORDLimits::radix == 2);
  static_assert(DWORDLimits::digits == 32);
  // This cast is guaranteed to do the right thing in C++20, and in practice
  // also works on older C++ versions.
  const int code_int = static_cast<int>(code);
  // Ensure we don’t accidentally return success on failure.
  CHECK((code == 0) == (code_int == 0));
  return code_int;
#else
  const std::vector<char* absl_nonnull> argv = Pointers(args);
  const std::vector<char* absl_nonnull> envp = Pointers(final_env);
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

absl::StatusOr<DosDevice> DosDevice::Create(
    [[maybe_unused]] const NativeStringView target) {
  CHECK(!target.empty());
#ifdef _WIN32
  const DWORD drives = ::GetLogicalDrives();
  if (drives == 0) return WindowsStatus("GetLogicalDrives");
  for (wchar_t letter = L'Z'; letter >= L'D'; --letter) {
    const DWORD bit = 1U << (letter - L'A');
    if ((drives & bit) == 0) {
      constexpr DWORD flags = DDD_NO_BROADCAST_SYSTEM;
      const wchar_t name[] = {letter, L':', L'\0'};
      std::wstring string(target);
      if (!::DefineDosDeviceW(flags, name, Pointer(string))) {
        return WindowsStatus("DefineDosDeviceW", flags, Escape(name),
                             Escape(target));
      }
      return DosDevice(name, target);
    }
  }
  return absl::ResourceExhaustedError("no drive letters available");
#else
  return absl::UnimplementedError("this system doesn’t support DOS devices");
#endif
}

DosDevice::~DosDevice() noexcept {
#ifdef _WIN32
  if (name_.empty()) return;
  constexpr DWORD flags = DDD_REMOVE_DEFINITION | DDD_EXACT_MATCH_ON_REMOVE |
                          DDD_NO_BROADCAST_SYSTEM;
  if (!::DefineDosDeviceW(flags, Pointer(name_), Pointer(target_))) {
    LOG(ERROR) << WindowsStatus("DefineDosDeviceW", flags, Escape(name_),
                                Escape(target_));
  }
#endif
}

}  // namespace rules_elisp
