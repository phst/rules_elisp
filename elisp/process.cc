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

#include <algorithm>  // IWYU pragma: keep
#include <cerrno>     // IWYU pragma: keep
#include <cstddef>
#include <cstdlib>
#include <limits>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <system_error>  // IWYU pragma: keep
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
#include "absl/base/optimization.h"
#include "absl/cleanup/cleanup.h"  // IWYU pragma: keep
#include "absl/container/flat_hash_map.h"
#include "absl/hash/hash.h"  // IWYU pragma: keep
#include "absl/log/check.h"
#include "absl/log/die_if_null.h"
#include "absl/log/log.h"  // IWYU pragma: keep
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

// IWYU pragma: no_include <__system_error/error_category.h>
// IWYU pragma: no_include <__system_error/error_code.h>
// IWYU pragma: no_include <__system_error/error_condition.h>
// IWYU pragma: no_include <sys/errno.h>

namespace rules_elisp {

namespace {

template <typename To, typename From>
[[nodiscard]] constexpr bool Overflow(const From n) {
  static_assert(std::is_integral_v<To>);
  static_assert(std::is_integral_v<From>);
  using ToLimits = std::numeric_limits<To>;
  using FromLimits = std::numeric_limits<From>;
  if constexpr (ToLimits::is_signed == FromLimits::is_signed) {
    return n < ToLimits::min() || n > ToLimits::max();
  }
  if constexpr (ToLimits::is_signed && !FromLimits::is_signed) {
    return n > static_cast<std::make_unsigned_t<To>>(ToLimits::max());
  }
  if constexpr (!ToLimits::is_signed && FromLimits::is_signed) {
    return n < 0 ||
           static_cast<std::make_unsigned_t<From>>(n) > ToLimits::max();
  }
}

template <typename To, typename From>
std::optional<To> CastNumberOpt(const From n) {
  return Overflow<To>(n) ? std::optional<To>() : static_cast<To>(n);
}

template <typename To, typename From>
absl::StatusOr<To> CastNumber(const From n) {
  using Limits = std::numeric_limits<To>;
  const std::optional<To> ret = CastNumberOpt<To>(n);
  if (!ret) {
    return absl::OutOfRangeError(absl::StrCat("Number ", n, " out of range [",
                                              Limits::min(), ", ",
                                              Limits::max(), "]"));
  }
  return *ret;
}

using NativeString = std::basic_string<NativeChar>;

struct CaseInsensitiveHash;
struct CaseInsensitiveEqual;

#ifdef _WIN32
static std::string Escape(const std::wstring_view string) {
  std::string result;
  result.reserve(string.length());
  for (const wchar_t ch : string) {
    const std::optional<unsigned char> u = CastNumberOpt<unsigned char>(ch);
    if (u && absl::ascii_isprint(*u)) {
      result.push_back(static_cast<char>(*u));
    } else {
      absl::StrAppend(&result, "\\u", absl::Hex(ch, absl::kZeroPad4));
    }
  }
  return result;
}
#else
static std::string_view Escape(
    const std::string_view string ABSL_ATTRIBUTE_LIFETIME_BOUND) {
  return string;
}
#endif

#ifdef _WIN32
static std::wstring ToUpper(std::wstring_view string);

struct CaseInsensitiveHash {
  std::size_t operator()(const std::wstring_view string) const {
    return absl::HashOf(ToUpper(string));
  }
};

struct CaseInsensitiveEqual {
  bool operator()(const std::wstring_view a, const std::wstring_view b) const {
    return ToUpper(a) == ToUpper(b);
  }
};

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
static absl::Nonnull<wchar_t*> Pointer(
    std::wstring& string ABSL_ATTRIBUTE_LIFETIME_BOUND) {
  CHECK_EQ(string.find(L'\0'), string.npos)
      << Escape(string) << " contains null character";
  return string.data();
}
#else
static std::vector<absl::Nonnull<char*>> Pointers(
    std::vector<std::string>& strings ABSL_ATTRIBUTE_LIFETIME_BOUND) {
  std::vector<absl::Nonnull<char*>> ptrs;
  for (std::string& s : strings) {
    CHECK_EQ(s.find('\0'), s.npos) << s << " contains null character";
    ptrs.push_back(s.data());
  }
  ptrs.push_back(nullptr);
  return ptrs;
}
#endif

static absl::Status MakeErrorStatus(const std::error_code& code,
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

template <typename... Ts>
absl::Status ErrorStatus(const std::error_code& code,
                         const std::string_view function, Ts&&... args) {
  return MakeErrorStatus(code, function,
                         absl::StrJoin(std::forward_as_tuple(args...), ", "));
}

#ifdef _WIN32
template <typename... Ts>
absl::Status WindowsStatus(const std::string_view function, Ts&&... args) {
  const absl::StatusOr<int> code = CastNumber<int>(::GetLastError());
  if (!code.ok()) return code.status();
  return ErrorStatus(std::error_code(*code, std::system_category()), function,
                     std::forward<Ts>(args)...);
}
#else
template <typename... Ts>
absl::Status ErrnoStatus(const std::string_view function, Ts&&... args) {
  return ErrorStatus(std::error_code(errno, std::system_category()), function,
                     std::forward<Ts>(args)...);
}
#endif

static constexpr unsigned int kMaxASCII{0x7F};

template <typename Char>
absl::Status CheckASCII(const std::basic_string_view<Char> string) {
  const auto it = absl::c_find_if_not(string, [](const Char ch) {
    return ch >= Char{0} && ch <= Char{kMaxASCII};
  });
  if (it != string.end()) {
    const auto val = static_cast<std::make_unsigned_t<Char>>(*it);
    return absl::InvalidArgumentError(
        absl::StrCat("non-ASCII character U+", absl::Hex(val, absl::kZeroPad4),
                     " in string"));
  }
  return absl::OkStatus();
}

// Convert strings between std::string and std::wstring.  This is only useful on
// Windows, where the native string type is std::wstring.  Only pure ASCII
// strings are supported so that we don’t have to deal with codepages.  All
// Windows codepages should be ASCII-compatible;
// cf. https://docs.microsoft.com/en-us/windows/win32/intl/code-pages.
template <typename ToString, typename FromChar>
absl::StatusOr<ToString> ConvertString(
    const std::basic_string_view<FromChar> string) {
  using ToChar = typename ToString::value_type;
  if constexpr (std::is_same_v<FromChar, ToChar>) return ToString(string);
  static_assert(!Overflow<ToChar>(kMaxASCII),
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
  return ConvertString<std::string>(string);
}

static absl::StatusOr<NativeString> ToNative(const std::string_view string) {
  return ConvertString<NativeString>(string);
}

#ifndef _WIN32
static absl::StatusOr<std::string> WorkingDirectory() {
  struct Free {
    void operator()(const absl::Nonnull<char*> p) const { std::free(p); }
  };
  // Assume that we always run on an OS that allocates a buffer when passed a
  // null pointer.
  const absl::Nullable<std::unique_ptr<char, Free>> ptr(getcwd(nullptr, 0));
  if (ptr == nullptr) return ErrnoStatus("getcwd", nullptr, 0);
  // See the Linux man page for getcwd(3) why this can happen.
  if (*ptr != '/') {
    return absl::NotFoundError(absl::StrCat("Current working directory ",
                                            ptr.get(), " is unreachable"));
  }
  return ptr.get();
}
#endif

class EnvironmentBlock final {
 private:
  struct Free;
#ifdef _WIN32
  struct Free {
    void operator()(const absl::Nonnull<wchar_t*> p) const noexcept {
      const BOOL ok = ::FreeEnvironmentStringsW(p);
      // If this fails, we can’t really do much except logging the error.
      if (!ok) LOG(ERROR) << WindowsStatus("FreeEnvironmentStringsW");
    }
  };
#endif

  using Pointer = std::conditional_t<Windows, std::unique_ptr<wchar_t, Free>,
                                     const absl::Nullable<const char*>*>;

 public:
  static absl::StatusOr<EnvironmentBlock> Current() {
#ifdef _WIN32
    absl::Nullable<Pointer> envp(::GetEnvironmentStringsW());
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
    using Environ = absl::Nonnull<const absl::Nullable<const char*>*>;
    using Pointer =
        std::conditional_t<Windows, absl::Nonnull<const wchar_t*>, Environ>;

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
    std::conditional_t<Windows, std::wstring_view, Environ> cur_;

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
  explicit EnvironmentBlock(absl::Nonnull<Pointer> ptr)
      : ptr_(std::move(ABSL_DIE_IF_NULL(ptr))) {}

  absl::Nonnull<Pointer> ptr_;
};

using Environment = std::conditional_t<
    Windows,
    absl::flat_hash_map<std::wstring, std::wstring, CaseInsensitiveHash,
                        CaseInsensitiveEqual>,
    absl::flat_hash_map<std::string, std::string>>;

static absl::StatusOr<Environment> CopyEnv() {
  const absl::StatusOr<EnvironmentBlock> block = EnvironmentBlock::Current();
  if (!block.ok()) return block.status();
  Environment map;
  // Skip over the first character to properly deal with the magic “per-drive
  // current directory” variables on Windows,
  // cf. https://devblogs.microsoft.com/oldnewthing/20100506-00/?p=14133.  Their
  // names start with an equals sign.
  constexpr std::size_t skip = Windows ? 1 : 0;
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
  return map;
}

using bazel::tools::cpp::runfiles::Runfiles;
using RunfilesPtr = absl::Nonnull<std::unique_ptr<Runfiles>>;

static absl::Nullable<Runfiles*> CreateRunfiles(
    const ExecutableKind kind, const std::string& argv0,
    const std::string& source_repository, std::string& error) {
  switch (kind) {
    case ExecutableKind::kBinary:
      return bazel::tools::cpp::runfiles::Runfiles::Create(
          argv0, source_repository, &error);
    case ExecutableKind::kTest:
      return bazel::tools::cpp::runfiles::Runfiles::CreateForTest(
          source_repository, &error);
  }
  LOG(FATAL) << "invalid runfiles mode "
             << static_cast<std::underlying_type_t<ExecutableKind>>(kind);
  ABSL_UNREACHABLE();
}

static absl::StatusOr<RunfilesPtr> CreateRunfiles(
    const ExecutableKind kind, const std::string_view source_repository,
    const absl::Span<const NativeStringView> original_argv) {
  if (const absl::Status status = CheckASCII(source_repository); !status.ok()) {
    return status;
  }
  const absl::StatusOr<std::string> argv0 =
      original_argv.empty() ? std::string() : ToNarrow(original_argv.front());
  if (!argv0.ok()) return argv0.status();
  std::string error;
  absl::Nullable<std::unique_ptr<Runfiles>> runfiles(
      CreateRunfiles(kind, *argv0, std::string(source_repository), error));
  if (runfiles == nullptr) {
    return absl::FailedPreconditionError(
        absl::StrCat("couldn’t create runfiles: ", error));
  }
  return runfiles;
}

static absl::StatusOr<NativeString> ResolveRunfile(
    const Runfiles& runfiles, const std::string_view name) {
  if (const absl::Status status = CheckASCII(name); !status.ok()) return status;
  std::string resolved = runfiles.Rlocation(std::string(name));
  if (resolved.empty()) {
    return absl::NotFoundError(absl::StrCat("runfile not found: ", name));
  }
#ifdef _WIN32
  absl::c_replace(resolved, '/', '\\');
#endif
  absl::StatusOr<NativeString> native = ToNative(resolved);
  if (!native.ok()) return native.status();
#ifdef _WIN32
  // 0x8000 is the maximum length of a filename on Windows.  See
  // https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfullpathnamew#parameters.
  constexpr DWORD size = 0x8000;
  std::wstring buffer(size, L'\0');
  const DWORD result =
      ::GetFullPathNameW(Pointer(*native), size, buffer.data(), nullptr);
  if (result == 0) {
    return WindowsStatus("GetFullPathNameW", Escape(*native), size, "...",
                         nullptr);
  }
  if (result > size) {
    return absl::OutOfRangeError(
        absl::StrCat("Length of absolute file name (", result,
                     ") overflows buffer of size ", size));
  }
  return buffer.substr(0, result);
#else
  CHECK(!native->empty());
  if (native->front() == '/') return native;
  const absl::StatusOr<std::string> cwd = WorkingDirectory();
  if (!cwd.ok()) return cwd.status();
  return absl::StrCat(*cwd, "/", *native);
#endif
}

static absl::StatusOr<Environment> RunfilesEnvironment(
    const Runfiles& runfiles) {
  Environment map;
  for (const auto& [narrow_key, narrow_value] : runfiles.EnvVars()) {
    const absl::StatusOr<NativeString> key = ToNative(narrow_key);
    if (!key.ok()) return key.status();
    const absl::StatusOr<NativeString> value = ToNative(narrow_value);
    if (!value.ok()) return value.status();
    const auto [it, ok] = map.emplace(*key, *value);
    if (!ok) {
      return absl::AlreadyExistsError(
          absl::StrCat("Duplicate runfiles environment variable ", narrow_key));
    }
  }
  return map;
}

static absl::StatusOr<int> Run(std::vector<NativeString>& args,
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
  const auto close_handles = absl::MakeCleanup([&process_info] {
    ::CloseHandle(process_info.hProcess);
    ::CloseHandle(process_info.hThread);
  });
  const DWORD status = ::WaitForSingleObject(process_info.hProcess, INFINITE);
  if (status != WAIT_OBJECT_0) return WindowsStatus("WaitForSingleObject");
  DWORD code;
  success = ::GetExitCodeProcess(process_info.hProcess, &code);
  if (!success) return WindowsStatus("GetExitCodeProcess");
  return CastNumber<int>(code);
#else
  const std::vector<absl::Nonnull<char*>> argv = Pointers(args);
  const std::vector<absl::Nonnull<char*>> envp = Pointers(final_env);
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

}  // namespace

absl::StatusOr<int> Run(
    const std::string_view source_repository, const std::string_view binary,
    const std::initializer_list<NativeStringView> common_args,
    const std::initializer_list<NativeStringView> launcher_args,
    const absl::Span<const NativeStringView> original_args,
    const ExecutableKind kind) {
  const absl::StatusOr<RunfilesPtr> runfiles =
      CreateRunfiles(kind, source_repository, original_args);
  if (!runfiles.ok()) return runfiles.status();
  absl::StatusOr<NativeString> resolved_binary =
      ResolveRunfile(**runfiles, binary);
  if (!resolved_binary.ok()) return resolved_binary.status();
  std::vector<NativeString> final_args{*resolved_binary};
  final_args.insert(final_args.end(), common_args.begin(), common_args.end());
  final_args.insert(final_args.end(), launcher_args.begin(),
                    launcher_args.end());
  final_args.push_back(RULES_ELISP_NATIVE_LITERAL("--"));
  final_args.insert(final_args.end(), original_args.begin(),
                    original_args.end());
  absl::StatusOr<Environment> merged_env = RunfilesEnvironment(**runfiles);
  if (!merged_env.ok()) return merged_env.status();
  absl::StatusOr<Environment> orig_env = CopyEnv();
  if (!orig_env.ok()) return orig_env.status();
  // We don’t want the Python launcher to change the current working directory,
  // otherwise relative filenames will be all messed up.  See
  // https://github.com/bazelbuild/bazel/issues/7190.
  orig_env->erase(RULES_ELISP_NATIVE_LITERAL("RUN_UNDER_RUNFILES"));
  merged_env->insert(orig_env->begin(), orig_env->end());
  return Run(final_args, *merged_env);
}

namespace {

#ifdef _WIN32
static std::wstring ToUpper(const std::wstring_view string) {
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
#endif

}  // namespace

}  // namespace rules_elisp
