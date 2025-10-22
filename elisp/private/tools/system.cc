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
#  include <dirent.h>
#  include <limits.h>
#  include <spawn.h>
#  include <stdio.h>
#  include <stdlib.h>
#  include <sys/stat.h>
#  include <sys/types.h>
#  include <sys/wait.h>
#  include <unistd.h>
#endif

#ifdef __APPLE__
#  include <crt_externs.h>  // for _NSGetEnviron
#endif

#include <algorithm>  // IWYU pragma: keep, only on Windows
#include <cerrno>
#include <cstddef>
#include <cstdint>  // IWYU pragma: keep, only on Windows
#include <cstdio>
#include <cstdlib>
#include <functional>
#include <memory>
#include <optional>  // IWYU pragma: keep, only on Windows
#include <string>
#include <string_view>
#include <system_error>
#include <type_traits>
#include <utility>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/base/attributes.h"
#include "absl/base/nullability.h"
#include "absl/cleanup/cleanup.h"
#include "absl/container/hash_container_defaults.h"
#include "absl/log/check.h"
#include "absl/log/die_if_null.h"
#include "absl/log/log.h"  // IWYU pragma: keep, only on Windows
#include "absl/status/status.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"
#include "absl/time/clock.h"  // IWYU pragma: keep, only on Windows
#include "absl/time/time.h"
#include "absl/types/span.h"

#include "elisp/private/tools/numeric.h"  // IWYU pragma: keep, only on Windows
#include "elisp/private/tools/platform.h"
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
  const std::optional<int> i = CastNumber<int>(code);
  return i.has_value() ? std::error_code(*i, std::system_category())
                       : std::make_error_code(std::errc::value_too_large);
#else
  return std::make_error_code(std::errc::operation_not_supported);
#endif
}

[[nodiscard]] std::error_code ErrnoError() {
  const int code = errno;
  return std::error_code(code, std::generic_category());
}

#ifdef _WIN32
// Build a command line that follows the Windows conventions.  See
// https://docs.microsoft.com/en-us/cpp/cpp/main-function-command-line-args?view=msvc-170#parsing-c-command-line-arguments
// and
// https://docs.microsoft.com/en-us/windows/win32/api/shellapi/nf-shellapi-commandlinetoargvw.
static absl::StatusOr<std::wstring> BuildCommandLine(
    const absl::Span<const std::wstring> args) {
  std::wstring result;
  bool first = true;
  for (const std::wstring& arg : args) {
    if (ContainsNull(arg)) {
      return absl::InvalidArgumentError(
          absl::StrFormat("Argument %s contains null character", arg));
    }
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
static absl::StatusOr<std::wstring> BuildEnvironmentBlock(
    const absl::Span<const std::wstring> vars) {
  std::wstring result;
  for (const std::wstring& var : vars) {
    if (ContainsNull(var)) {
      return absl::InvalidArgumentError(absl::StrFormat(
          "Environment variable %s contains null character", var));
    }
    result.append(var);
    result.push_back(L'\0');
  }
  result.push_back(L'\0');
  return result;
}
#endif

static NativeChar* absl_nonnull Pointer(
    NativeString& string ABSL_ATTRIBUTE_LIFETIME_BOUND) {
  CHECK(!ContainsNull(string))
      << absl::StrFormat("%s contains null character", string);
  return string.data();
}

#ifndef _WIN32
static std::vector<char* absl_nullable> Pointers(
    std::vector<std::string>& strings ABSL_ATTRIBUTE_LIFETIME_BOUND) {
  std::vector<char* absl_nullable> ptrs;
  for (std::string& s : strings) {
    CHECK(!ContainsNull(s)) << s << " contains null character";
    ptrs.push_back(s.data());
  }
  ptrs.push_back(nullptr);
  return ptrs;
}
#endif

absl::StatusOr<std::string> ToNarrow(const NativeStringView string,
                                     [[maybe_unused]] const Encoding encoding) {
  if (string.empty()) return std::string();
#ifdef _WIN32
  if (encoding == Encoding::kAscii) {
    // Windows doesn’t support the ASCII codepage with WC_ERR_INVALID_CHARS.  So
    // we check for non-ASCII first and use UTF-8 (as superset of ASCII) in all
    // cases.
    const absl::Status status = CheckAscii(string);
    if (!status.ok()) return status;
  }
  constexpr UINT codepage = CP_UTF8;
  constexpr unsigned int max_bytes_per_wchar = 3;
  constexpr DWORD flags = WC_ERR_INVALID_CHARS;
  const std::optional<std::string::size_type> buffer_size =
      Multiply<std::string::size_type>(string.length(), max_bytes_per_wchar);
  if (!buffer_size.has_value()) {
    return absl::InvalidArgumentError(
        absl::StrFormat("String too long (%d code units)", string.length()));
  }
  const std::optional<int> wide_length = CastNumber<int>(string.length());
  if (!wide_length.has_value()) {
    return absl::InvalidArgumentError(
        absl::StrFormat("String too long (%d code units)", string.length()));
  }
  const std::optional<int> narrow_length = CastNumber<int>(*buffer_size);
  if (!narrow_length.has_value()) {
    return absl::InvalidArgumentError(
        absl::StrFormat("String too long (%d code units)", string.length()));
  }
  std::string buffer(*buffer_size, '\0');
  const int result =
      ::WideCharToMultiByte(codepage, flags, string.data(), *wide_length,
                            buffer.data(), *narrow_length, nullptr, nullptr);
  if (result == 0) {
    return WindowsStatus("WideCharToMultiByte", codepage, flags, "...",
                         *wide_length, "...", *narrow_length, nullptr, nullptr);
  }
  return buffer.substr(0, CastNumber<std::string::size_type>(result).value());
#else
  return std::string(string);
#endif
}

absl::StatusOr<NativeString> ToNative(
    const std::string_view string, [[maybe_unused]] const Encoding encoding) {
  if (string.empty()) return NativeString();
#ifdef _WIN32
  if (encoding == Encoding::kAscii) {
    // Windows doesn’t support the ASCII codepage with MB_ERR_INVALID_CHARS.  So
    // we check for non-ASCII first and use UTF-8 (as superset of ASCII) in all
    // cases.
    const absl::Status status = CheckAscii(string);
    if (!status.ok()) return status;
  }
  constexpr UINT codepage = CP_UTF8;
  constexpr DWORD flags = MB_ERR_INVALID_CHARS;
  const std::optional<int> length = CastNumber<int>(string.length());
  if (!length.has_value()) {
    return absl::InvalidArgumentError(
        absl::StrFormat("String too long (%d bytes)", string.length()));
  }
  NativeString buffer(string.length(), L'\0');
  const int result = ::MultiByteToWideChar(codepage, flags, string.data(),
                                           *length, buffer.data(), *length);
  if (result == 0) {
    return WindowsStatus("MultiByteToWideChar", codepage, flags, "...", *length,
                         "...", *length);
  }
  return buffer.substr(0, CastNumber<NativeString::size_type>(result).value());
#else
  return NativeString(string);
#endif
}

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

bool IsAbsolute(const NativeStringView file) {
  if (file.empty()) return false;
  if constexpr (kWindows) {
    if (file.length() < 3) return false;
    return file.length() > 2 && file[1] == RULES_ELISP_NATIVE_LITERAL(':') &&
           (file[2] == RULES_ELISP_NATIVE_LITERAL('\\') ||
            file[2] == RULES_ELISP_NATIVE_LITERAL('/'));
  } else {
    return file.front() == RULES_ELISP_NATIVE_LITERAL('/');
  }
}

absl::StatusOr<NativeString> MakeAbsolute(const NativeStringView file) {
  if (file.empty()) return absl::InvalidArgumentError("empty filename");
  if (ContainsNull(file)) {
    return absl::InvalidArgumentError(
        absl::StrFormat("Filename %s contains null character", file));
  }
#ifdef _WIN32
  std::wstring string(file);
  // 0x8000 is the maximum length of a filename on Windows.  See
  // https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfullpathnamew#parameters.
  constexpr DWORD size{0x8000};
  std::wstring buffer(size, L'\0');
  const DWORD result =
      ::GetFullPathNameW(Pointer(string), size, buffer.data(), nullptr);
  if (result == 0) {
    return WindowsStatus("GetFullPathNameW", file, size, "...", nullptr);
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

[[nodiscard]] static bool ConsumePrefix(NativeStringView& string,
                                        const NativeStringView prefix) {
  const NativeStringView::size_type n = prefix.length();
  if (string.substr(0, n) != prefix) return false;
  string.remove_prefix(n);
  return true;
}

absl::StatusOr<NativeString> MakeRelative(const NativeStringView file,
                                          const NativeStringView base) {
  if (file.empty()) return absl::InvalidArgumentError("Empty filename");
  if (ContainsNull(file)) {
    return absl::InvalidArgumentError(
        absl::StrFormat("Filename %s contains null character", file));
  }
  if (base.empty()) return absl::InvalidArgumentError("Empty base directory");
  if (ContainsNull(base)) {
    return absl::InvalidArgumentError(
        absl::StrFormat("Base directory %s contains null character", file));
  }
  NativeString file_str(file);
  NativeString base_str(base);
  if constexpr (kWindows) {
    absl::c_replace(file_str, RULES_ELISP_NATIVE_LITERAL('/'), kSeparator);
    absl::c_replace(base_str, RULES_ELISP_NATIVE_LITERAL('/'), kSeparator);
  }
  if (base_str.back() != kSeparator) base_str.push_back(kSeparator);
  constexpr NativeStringView kTwoSeparators =
      kWindows ? RULES_ELISP_NATIVE_LITERAL("\\\\")
               : RULES_ELISP_NATIVE_LITERAL("//");
  if (file_str.find(kTwoSeparators) != file_str.npos) {
    return absl::InvalidArgumentError(
        absl::StrFormat("Filename %s isn’t canonical", file_str));
  }
  if (base_str.find(kTwoSeparators) != base_str.npos) {
    return absl::InvalidArgumentError(
        absl::StrFormat("Filename %s isn’t canonical", file_str));
  }
  NativeStringView rel = file_str;
  if (!ConsumePrefix(rel, base_str) || rel.empty()) {
    return absl::InvalidArgumentError(
        absl::StrFormat("File %s is not within %s", file_str, base_str));
  }
  return NativeString(rel);
}

[[nodiscard]] bool FileExists(const NativeStringView file) {
  if (file.empty() || ContainsNull(file)) return false;
  NativeString string(file);
#ifdef _WIN32
  return ::GetFileAttributesW(Pointer(string)) != INVALID_FILE_ATTRIBUTES;
#else
  struct stat st;
  return lstat(Pointer(string), &st) == 0;
#endif
}

static absl::Status IterateDirectory(
    const NativeStringView dir,
    const std::function<bool(NativeStringView)>& function) {
  if (dir.empty()) return absl::InvalidArgumentError("Empty directory name");
  if (ContainsNull(dir)) {
    return absl::InvalidArgumentError(
        absl::StrFormat("Directory name %s contains null character", dir));
  }
#ifdef _WIN32
  std::wstring pattern = std::wstring(dir) + L"\\*";
  WIN32_FIND_DATAW data;
  const HANDLE handle = ::FindFirstFileW(Pointer(pattern), &data);
  if (handle == INVALID_HANDLE_VALUE) {
    if (::GetLastError() != ERROR_FILE_NOT_FOUND) {
      return WindowsStatus("FindFirstFileW", pattern);
    }
    return absl::OkStatus();
  }
  const absl::Cleanup cleanup = [handle]() {
    if (!::FindClose(handle)) LOG(ERROR) << WindowsStatus("FindClose");
  };
  do {
    const std::wstring_view name = data.cFileName;
    if (name == L"." || name == L"..") continue;
    if (!function(name)) return absl::OkStatus();
  } while (::FindNextFileW(handle, &data));
  if (::GetLastError() != ERROR_NO_MORE_FILES) {
    return WindowsStatus("FindNextFileW");
  }
#else
  std::string string(dir);
  DIR* const absl_nullable handle = opendir(Pointer(string));
  if (handle == nullptr) return ErrnoStatus("opendir", dir);
  const absl::Cleanup cleanup = [handle]() {
    if (closedir(handle) != 0) LOG(ERROR) << ErrnoStatus("closedir");
  };
  while (true) {
    errno = 0;
    const struct dirent* const absl_nullable entry = readdir(handle);
    if (entry == nullptr) {
      if (errno != 0) return ErrnoStatus("readdir");
      break;
    }
    const std::string_view name = entry->d_name;
    if (name == "." || name == "..") continue;
    if (!function(name)) break;
  }
#endif
  return absl::OkStatus();
}

[[nodiscard]] bool IsNonEmptyDirectory(const NativeStringView directory) {
  bool empty = true;
  const absl::Status status =
      IterateDirectory(directory, [&empty](NativeStringView) {
        empty = false;
        return false;
      });
  return status.ok() && !empty;
}

#undef CreateDirectory

absl::Status CreateDirectory(const NativeStringView name) {
  if (name.empty()) return absl::InvalidArgumentError("Empty directory name");
  if (ContainsNull(name)) {
    return absl::InvalidArgumentError(
        absl::StrFormat("Directory name %s contains null character", name));
  }
  NativeString string(name);
#ifdef _WIN32
  const BOOL ok = ::CreateDirectoryW(Pointer(string), nullptr);
  if (!ok) return WindowsStatus("CreateDirectoryW", name);
#else
  constexpr mode_t mode = S_IRWXU;
  const int result = mkdir(Pointer(string), mode);
  if (result != 0) return ErrnoStatus("mkdir", name, mode);
#endif
  return absl::OkStatus();
}

#undef RemoveDirectory

absl::Status RemoveDirectory(const NativeStringView name) {
  if (name.empty()) return absl::InvalidArgumentError("Empty directory name");
  if (ContainsNull(name)) {
    return absl::InvalidArgumentError(
        absl::StrFormat("Directory name %s contains null character", name));
  }
  NativeString string(name);
#ifdef _WIN32
  const BOOL ok = ::RemoveDirectoryW(Pointer(string));
  if (!ok) return WindowsStatus("RemoveDirectoryW", name);
#else
  const int result = rmdir(Pointer(string));
  if (result != 0) return ErrnoStatus("rmdir", name);
#endif
  return absl::OkStatus();
}

absl::StatusOr<std::vector<NativeString>> ListDirectory(
    const NativeStringView dir) {
  std::vector<NativeString> result;
  const absl::Status status =
      IterateDirectory(dir, [&result](const NativeStringView file) {
        result.emplace_back(file);
        return true;
      });
  if (!status.ok()) return status;
  return result;
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
          absl::StrFormat("Invalid environment block entry %s", var));
    }
    const std::size_t i = var.find(RULES_ELISP_NATIVE_LITERAL('='), skip);
    if (i == 0 || i == var.npos) {
      return absl::FailedPreconditionError(
          absl::StrFormat("Invalid environment block entry %s", var));
    }
    const NativeStringView key = var.substr(0, i);
    const auto [it, ok] = map.emplace(key, var.substr(i + 1));
    if (!ok) {
      return absl::AlreadyExistsError(
          absl::StrFormat("Duplicate environment variable %s", key));
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
  const int length = CastNumber<int>(string.length()).value();
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

absl::Status Unlink(const NativeStringView file) {
  if (file.empty()) return absl::InvalidArgumentError("Empty filename");
  if (ContainsNull(file)) {
    return absl::InvalidArgumentError(
        absl::StrFormat("Filename %s contains null character", file));
  }
  NativeString string(file);
#ifdef _WIN32
  const BOOL result = ::DeleteFileW(Pointer(string));
  if (!result) return WindowsStatus("DeleteFileW", file);
#else
  const int result = unlink(Pointer(string));
  if (result != 0) return ErrnoStatus("unlink", file);
#endif
  return absl::OkStatus();
};

absl::StatusOr<TemporaryFile> TemporaryFile::Create() {
#ifdef _WIN32
  absl::Status status;
  for (int i = 0; i < 10; i++) {
    wchar_t buffer[L_tmpnam + 1];
    const wchar_t* absl_nullable name = _wtmpnam(buffer);
    if (name == nullptr) {
      return absl::UnavailableError("Cannot create temporary name");
    }
    constexpr wchar_t mode[] = L"wxbNT";
    std::FILE* const absl_nullable file = _wfopen(name, mode);
    if (file != nullptr) return TemporaryFile(name, file);
    status = ErrnoStatus("_wfopen", name, mode);
    LOG(ERROR) << status;
  }
  CHECK(!status.ok());
  return status;
#else
  const char* const absl_nullable dir = std::getenv("TMPDIR");
  std::string name = absl::StrCat(dir == nullptr || *dir == '\0' ? "/tmp" : dir,
                                  "/elisp.XXXXXX");
  const int fd = mkstemp(Pointer(name));
  if (fd < 0) return ErrnoStatus("mkstemp", name);
  constexpr char mode[] = "wxbe";
  std::FILE* const absl_nullable file = fdopen(fd, mode);
  if (file == nullptr) {
    const absl::Status status = ErrnoStatus("fdopen", fd, mode);
    if (close(fd) != 0) LOG(ERROR) << ErrnoStatus("close", fd);
    const absl::Status unlink_status = Unlink(name);
    if (!unlink_status.ok()) LOG(ERROR) << unlink_status;
    return status;
  }
  return TemporaryFile(std::move(name), file);
#endif
}

TemporaryFile::~TemporaryFile() noexcept {
  if (name_.empty()) return;
  CHECK(file_ != nullptr);
  if (std::fclose(file_) != 0) LOG(FATAL) << ErrnoStatus("fclose");
  const absl::Status unlink_status = Unlink(name_);
  if (!unlink_status.ok()) LOG(ERROR) << unlink_status;
}

absl::Status TemporaryFile::Write(const std::string_view contents) {
  if (!contents.empty()) {
    constexpr std::size_t size = 1;
    const std::size_t count = contents.length();
    const std::size_t written =
        std::fwrite(contents.data(), size, count, file_);
    if (written != contents.size()) {
      const absl::Status status = ErrnoStatus("fwrite", "...", size, count);
      LOG(ERROR) << status << "; only " << written << " bytes of " << count
                 << " written";
      return status;
    }
  }
  if (std::fflush(file_) != 0) return ErrnoStatus("fflush");
  return absl::OkStatus();
}

absl::StatusOr<int> Run(const absl::Span<const NativeString> args,
                        const Environment& env, const absl::Time deadline) {
  if (args.empty()) return absl::InvalidArgumentError("Empty argument array");
  for (const NativeString& arg : args) {
    if (ContainsNull(arg)) {
      return absl::InvalidArgumentError(
          absl::StrFormat("Argument %s contains null character", arg));
    }
  }
  std::vector<NativeString> final_env;
  for (const auto& [key, value] : env) {
    final_env.push_back(key + RULES_ELISP_NATIVE_LITERAL('=') + value);
  }
  // Sort entries for hermeticity.
  absl::c_sort(final_env);
  const bool has_deadline = deadline < absl::InfiniteFuture();
#ifdef _WIN32
  std::wstring program = args.front();
  absl::StatusOr<std::wstring> command_line = BuildCommandLine(args);
  if (!command_line.ok()) return command_line.status();
  constexpr BOOL kInheritHandles = FALSE;
  const DWORD flags = CREATE_UNICODE_ENVIRONMENT |
                      (has_deadline ? CREATE_NEW_PROCESS_GROUP : 0);
  absl::StatusOr<std::wstring> envp = BuildEnvironmentBlock(final_env);
  if (!envp.ok()) return envp.status();
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
      ::CreateProcessW(Pointer(program), Pointer(*command_line), nullptr,
                       nullptr, kInheritHandles, flags, envp->data(), nullptr,
                       &startup_info, &process_info);
  if (!success) {
    return WindowsStatus("CreateProcessW", program, *command_line, nullptr,
                         nullptr, kInheritHandles, flags);
  }
  success = ::CloseHandle(process_info.hThread);
  if (!success) LOG(ERROR) << WindowsStatus("CloseHandle");
  const absl::Cleanup close_handle = [&process_info] {
    const BOOL success = ::CloseHandle(process_info.hProcess);
    if (!success) LOG(ERROR) << WindowsStatus("CloseHandle");
  };
  const DWORD timeout_ms =
      has_deadline ? static_cast<DWORD>(std::clamp(
                         absl::ToInt64Milliseconds(deadline - absl::Now()),
                         std::int64_t{0}, std::int64_t{MAXDWORD}))
                   : INFINITE;
  switch (::WaitForSingleObject(process_info.hProcess, timeout_ms)) {
    case WAIT_OBJECT_0:
      break;
    case WAIT_TIMEOUT:
      LOG(WARNING) << "Process timed out, sending CTRL + BREAK";
      success = ::GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT,
                                           process_info.dwProcessId);
      if (!success) {
        LOG(ERROR) << WindowsStatus("GenerateConsoleCtrlEvent",
                                    CTRL_BREAK_EVENT);
      }
      return absl::DeadlineExceededError(absl::StrFormat(
          "Deadline %v exceeded waiting for process (timeout %v)", deadline,
          absl::Milliseconds(timeout_ms)));
    default:
      return WindowsStatus("WaitForSingleObject", "...", timeout_ms);
  }
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
  if (has_deadline) {
    return absl::UnimplementedError(
        absl::StrFormat("Finite deadline %v unsupported", deadline));
  }
  std::vector<NativeString> args_vec(args.cbegin(), args.cend());
  const std::vector<char* absl_nullable> argv = Pointers(args_vec);
  const std::vector<char* absl_nullable> envp = Pointers(final_env);
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
  if (target.empty()) return absl::InvalidArgumentError("Empty target");
  if (ContainsNull(target)) {
    return absl::InvalidArgumentError(
        absl::StrFormat("Target %s contains null character", target));
  }
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
        return WindowsStatus("DefineDosDeviceW", flags, name, target);
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
    LOG(ERROR) << WindowsStatus("DefineDosDeviceW", flags, name_, target_);
  }
#endif
}

}  // namespace rules_elisp
