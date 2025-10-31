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
#  include <pathcch.h>
#  include <shellapi.h>
#  include <shlwapi.h>
#else
#  include <dirent.h>
#  include <fcntl.h>
#  include <ftw.h>
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
#  include <copyfile.h>     // for copyfile
#  include <crt_externs.h>  // for _NSGetEnviron
#endif

#undef StrCat

#include <algorithm>  // IWYU pragma: keep, only on Windows
#include <array>      // IWYU pragma: keep, only on Windows
#include <cerrno>
#include <cstddef>
#include <cstdint>  // IWYU pragma: keep, only on Windows
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <fstream>
#include <ios>
#include <iostream>
#include <locale>
#include <optional>
#include <ostream>
#include <sstream>
#include <string>
#include <string_view>
#include <system_error>
#include <type_traits>
#include <utility>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/base/attributes.h"
#include "absl/base/no_destructor.h"  // IWYU pragma: keep, only on Windows
#include "absl/base/nullability.h"
#include "absl/cleanup/cleanup.h"
#include "absl/container/hash_container_defaults.h"
#include "absl/log/check.h"
#include "absl/log/die_if_null.h"
#include "absl/log/log.h"
#include "absl/random/random.h"  // IWYU pragma: keep, only on Windows
#include "absl/status/status.h"
#include "absl/strings/ascii.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"
#include "absl/strings/str_split.h"
#include "absl/time/clock.h"  // IWYU pragma: keep, only on Windows
#include "absl/time/time.h"
#include "absl/types/span.h"

#include "elisp/private/tools/numeric.h"
#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/strings.h"

namespace rules_elisp {

namespace {

static absl::Status MakeErrorStatus(const std::error_code& code,
                                    const std::string_view function) {
  if (!code) return absl::OkStatus();
  const std::error_condition condition = code.default_error_condition();
  const std::string message =
      absl::StrCat(function, ": ", code.category().name(), "/", code.value(),
                   ": ", code.message());
  return condition.category() == std::generic_category()
             ? absl::ErrnoToStatus(condition.value(), message)
             : absl::UnknownError(message);
}

template <typename... Ts>
static absl::Status ErrorStatus(const std::error_code& code,
                                const absl::FormatSpec<Ts...>& format,
                                const Ts&... args) {
  return MakeErrorStatus(code, absl::StrFormat(format, args...));
}

[[nodiscard]] static std::error_code ErrnoError() {
  const int code = errno;
  return std::error_code(code, std::generic_category());
}

template <typename... Ts>
static absl::Status ErrnoStatus(const absl::FormatSpec<Ts...>& format,
                                const Ts&... args) {
  const std::error_code code = ErrnoError();
  return ErrorStatus(code, format, args...);
}

#ifdef _WIN32
[[nodiscard]] static std::error_code WindowsError() {
  const DWORD code = ::GetLastError();
  const std::optional<int> i = CastNumber<int>(code);
  return i.has_value() ? std::error_code(*i, std::system_category())
                       : std::make_error_code(std::errc::value_too_large);
}

template <typename... Ts>
static absl::Status WindowsStatus(const absl::FormatSpec<Ts...>& format,
                                  const Ts&... args) {
  const std::error_code code = WindowsError();
  return ErrorStatus(code, format, args...);
}

class HResultCategory final : public std::error_category {
 public:
  static const HResultCategory& Get() {
    static const absl::NoDestructor<HResultCategory> instance;
    return *instance;
  }

  const char* absl_nonnull name() const noexcept final {
    return "rules_elisp::HResultCategory";
  }

  std::string message(int val) const final {
    const HRESULT hr{val};
    std::string result = absl::StrFormat(
        "HRESULT %#010x (severity %d, facility %#06x, code %#06x)", hr,
        HRESULT_SEVERITY(hr), HRESULT_FACILITY(hr), HRESULT_CODE(hr));
    const std::optional<std::error_code> win32 = ToWin32(hr);
    if (win32.has_value()) absl::StrAppend(&result, "; ", win32->message());
    return result;
  };

  std::error_condition default_error_condition(int val) const noexcept final {
    const HRESULT hr{val};
    const std::optional<std::error_code> win32 = ToWin32(hr);
    return win32.has_value()
               ? win32->default_error_condition()
               : this->std::error_category::default_error_condition(val);
  }

  bool equivalent(const std::error_code& code, int val) const noexcept final {
    if (this->std::error_category::equivalent(code, val)) return true;
    const HRESULT hr{val};
    const std::optional<std::error_code> win32 = ToWin32(hr);
    return win32.has_value() && code == *win32;
  }

 private:
  static std::optional<std::error_code> ToWin32(const HRESULT hr) {
    if (hr == S_OK) return std::error_code();
    if (HRESULT_SEVERITY(hr) == SEVERITY_ERROR &&
        HRESULT_FACILITY(hr) == FACILITY_WIN32) {
      const int code{HRESULT_CODE(hr)};
      if (HRESULT_FROM_WIN32(code) == hr) {
        return std::error_code(code, std::system_category());
      }
    }
    return std::nullopt;
  }
};

[[nodiscard]] static std::error_code HResultError(const HRESULT hr) {
  return std::error_code(hr, HResultCategory::Get());
}

template <typename... Ts>
static absl::Status HResultStatus(const HRESULT hr,
                                  const absl::FormatSpec<Ts...>& format,
                                  const Ts&... args) {
  return ErrorStatus(HResultError(hr), format, args...);
}
#endif

}  // namespace

absl::StatusOr<FileName> FileName::FromString(const NativeStringView string) {
  if (string.empty()) return absl::InvalidArgumentError("Empty filename");
  if (ContainsNull(string)) {
    return absl::InvalidArgumentError(
        absl::StrFormat("Filename %s contains null character", string));
  }
  NativeString name(string);
  if constexpr (kWindows) {
    absl::c_replace(name, RULES_ELISP_NATIVE_LITERAL('/'), kSeparator);
    // Reject most of the “exotic” names from
    // https://googleprojectzero.blogspot.com/2016/02/the-definitive-guide-on-win32-to-nt.html.
    if (name.front() == kSeparator) {
      return absl::InvalidArgumentError(
          absl::StrFormat("Invalid filename %s", name));
    }
    const NativeString::size_type i =
        name.find(RULES_ELISP_NATIVE_LITERAL(':'));
    if (i != name.npos) {
      // Reject alternate data streams or drive-relative names.
      if (i != 1 || name.length() < 3 || name.at(2) != kSeparator) {
        return absl::InvalidArgumentError(
            absl::StrFormat("Invalid filename %s", name));
      }
      // Reject non-alphabetic drive letters.
      const std::optional<unsigned char> drive =
          CastNumber<unsigned char>(name.front());
      if (!drive.has_value() || !absl::ascii_isalpha(*drive)) {
        return absl::InvalidArgumentError(
            absl::StrFormat("Invalid filename %s", name));
      }
    }
  }
  return FileName(std::move(name));
}

#ifdef _WIN32
static const NativeChar* absl_nonnull Pointer(
    const NativeString& string ABSL_ATTRIBUTE_LIFETIME_BOUND);
#endif

absl::StatusOr<FileName> FileName::Parent() const {
  NativeString string = string_;
  while (string.back() == kSeparator) string.pop_back();
  NativeString::size_type i = string.rfind(kSeparator);
  if (i == string.npos || (kWindows && i == 0)) {
    return absl::InvalidArgumentError(
        absl::StrFormat("File %s has no parent", *this));
  }
  const NativeStringView view = string;
  const NativeStringView element = view.substr(i + 1);
  if (element == RULES_ELISP_NATIVE_LITERAL(".") ||
      element == RULES_ELISP_NATIVE_LITERAL("..")) {
    return absl::InvalidArgumentError(absl::StrFormat(
        "Removing trailing component %s would be ambiguous", element));
  }
#ifdef _WIN32
  std::array<wchar_t, PATHCCH_MAX_CCH> buffer;
  {
    constexpr ULONG flags = PATHCCH_NONE;
    const HRESULT hr = PathCchCanonicalizeEx(buffer.data(), buffer.size(),
                                             Pointer(string), flags);
    if (FAILED(hr)) {
      return HResultStatus(hr, "PathCchCanonializeEx(..., %#x, %#s, %#x)",
                           buffer.size(), string, flags);
    }
  }
  {
    PWSTR end;
    const HRESULT hr =
        PathCchRemoveBackslashEx(buffer.data(), buffer.size(), &end, nullptr);
    if (FAILED(hr)) {
      return HResultStatus(hr, "PathCchRemoveBackslashEx(%#s, %#x)",
                           buffer.data(), buffer.size());
    }
    if (*end != L'\0') {
      return absl::FailedPreconditionError(absl::StrFormat(
          "Cannot remove trailing backslash from %s", buffer.data()));
    }
  }
  {
    const HRESULT hr = PathCchRemoveFileSpec(buffer.data(), buffer.size());
    if (FAILED(hr)) {
      return HResultStatus(hr, "PathCchRemoveFileSpec(%#s, %#x)", buffer.data(),
                           buffer.size());
    }
  }
  return FileName::FromString(buffer.data());
#else
  // Root directories need to end in a separator character.
  return FileName::FromString(string.substr(0, i == 0 ? 1 : i));
#endif
}

absl::StatusOr<FileName> FileName::Child(const FileName& child) const {
  const NativeString& string = child.string_;
  if (string == RULES_ELISP_NATIVE_LITERAL(".") ||
      string == RULES_ELISP_NATIVE_LITERAL("..") ||
      string.find(kSeparator) != string.npos) {
    return absl::InvalidArgumentError(
        absl::StrFormat("File %s is not a child of %s", child, *this));
  }
  return this->Join(child);
}

absl::StatusOr<FileName> FileName::Child(const NativeStringView child) const {
  const absl::StatusOr<FileName> name = FileName::FromString(child);
  if (!name.ok()) return name.status();
  return this->Child(*name);
}

absl::StatusOr<FileName> FileName::Join(const FileName& descendant) const {
  if (descendant.IsAbsolute()) {
    return absl::InvalidArgumentError(
        absl::StrFormat("File name %s is absolute", descendant));
  }
#ifdef _WIN32
  std::array<wchar_t, PATHCCH_MAX_CCH> buffer;
  constexpr ULONG flags = PATHCCH_NONE;
  const HRESULT hr =
      ::PathCchCombineEx(buffer.data(), buffer.size(), this->pointer(),
                         descendant.pointer(), flags);
  if (FAILED(hr)) {
    return HResultStatus(hr, "PathCchCombineEx(..., %#x, %#s, %#s, %#x)",
                         buffer.size(), *this, descendant, flags);
  }
  return FileName::FromString(buffer.data());
#else
  return FileName::FromString(absl::StrCat(
      string_, string_.back() == '/' ? "" : "/", descendant.string_));
#endif
}

absl::StatusOr<FileName> FileName::Join(
    const NativeStringView descendant) const {
  const absl::StatusOr<FileName> name = FileName::FromString(descendant);
  if (!name.ok()) return name.status();
  return this->Join(*name);
}

absl::FormatConvertResult<absl::FormatConversionCharSet::s> AbslFormatConvert(
    const FileName& file, const absl::FormatConversionSpec& spec,
    absl::FormatSink* const absl_nonnull sink) {
  CHECK_EQ(spec.conversion_char(), absl::FormatConversionChar::s);
  CHECK(!spec.has_left_flag());
  CHECK(!spec.has_show_pos_flag());
  CHECK(!spec.has_sign_col_flag());
  CHECK(!spec.has_zero_flag());
  CHECK_LT(spec.width(), 0);
  CHECK_LT(spec.precision(), 0);
  if (spec.has_alt_flag()) {
    sink->Append(Quote(file.string_));
    return {true};
  } else {
    return {absl::Format(sink, "%s", file.string_)};
  }
}

void PrintTo(const FileName& file, std::ostream* const absl_nonnull stream) {
  *stream << absl::StreamFormat("%s", file.string());
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
      << absl::StreamFormat("%s contains null character", string);
  return string.data();
}

#ifdef _WIN32
static const NativeChar* absl_nonnull Pointer(
    const NativeString& string ABSL_ATTRIBUTE_LIFETIME_BOUND) {
  CHECK(!ContainsNull(string))
      << absl::StreamFormat("%s contains null character", string);
  return string.data();
}
#endif

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
    return WindowsStatus(
        "WideCharToMultiByte(%d, %#x, ..., %d, ..., %d, nullptr, nullptr)",
        codepage, flags, *wide_length, *narrow_length);
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
    return WindowsStatus("MultiByteToWideChar(%d, %#x, ..., %d, ..., %d)",
                         codepage, flags, *length, *length);
  }
  return buffer.substr(0, CastNumber<NativeString::size_type>(result).value());
#else
  return NativeString(string);
#endif
}

#ifndef _WIN32
static absl::StatusOr<FileName> WorkingDirectory() {
  // Assume that we always run on an OS that allocates a buffer when passed a
  // null pointer.
  char* const absl_nullable ptr = getcwd(nullptr, 0);
  if (ptr == nullptr) return ErrnoStatus("getcwd(nullptr, 0)");
  const absl::Cleanup cleanup = [ptr] { std::free(ptr); };
  // See the Linux man page for getcwd(3) why this can happen.
  if (*ptr != '/') {
    return absl::NotFoundError(
        absl::StrFormat("Current working directory %s is unreachable", ptr));
  }
  return FileName::FromString(ptr);
}
#endif

bool FileName::IsAbsolute() const {
  if constexpr (kWindows) {
    if (string_.length() < 3) return false;
    return string_.length() > 2 &&
           string_[1] == RULES_ELISP_NATIVE_LITERAL(':') &&
           (string_[2] == RULES_ELISP_NATIVE_LITERAL('\\') ||
            string_[2] == RULES_ELISP_NATIVE_LITERAL('/'));
  } else {
    return string_.front() == RULES_ELISP_NATIVE_LITERAL('/');
  }
}

absl::StatusOr<FileName> FileName::MakeAbsolute() const {
#ifdef _WIN32
  constexpr DWORD size{PATHCCH_MAX_CCH};
  wchar_t buffer[size];
  const DWORD result =
      ::GetFullPathNameW(this->pointer(), size, buffer, nullptr);
  if (result == 0) {
    return WindowsStatus("GetFullPathNameW(%#s, %#x, ..., nullptr)", *this,
                         size);
  }
  if (result > size) {
    return absl::OutOfRangeError(
        absl::StrCat("Length of absolute file name (", result,
                     ") overflows buffer of size ", size));
  }
  return FileName::FromString(std::wstring_view(buffer, result));
#else
  if (string_.front() == '/') return *this;
  const absl::StatusOr<FileName> cwd = WorkingDirectory();
  if (!cwd.ok()) return cwd.status();
  return cwd->Join(*this);
#endif
}

[[nodiscard]] static bool ConsumePrefix(NativeStringView& string,
                                        const NativeStringView prefix) {
  const NativeStringView::size_type n = prefix.length();
  if (string.substr(0, n) != prefix) return false;
  string.remove_prefix(n);
  return true;
}

absl::StatusOr<FileName> FileName::MakeRelative(const FileName& base) const {
  NativeString file_str = string_;
  NativeString base_str = base.string();
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
#ifdef _WIN32
  std::array<wchar_t, PATHCCH_MAX_CCH> buffer;
  if (!::PathRelativePathToW(buffer.data(), Pointer(base_str),
                             FILE_ATTRIBUTE_DIRECTORY, Pointer(file_str),
                             FILE_ATTRIBUTE_NORMAL)) {
    return absl::FailedPreconditionError(
        absl::StrFormat("Cannot make %s relative to %s", file_str, base_str));
  }
  return FileName::FromString(buffer.data());
#else
  return FileName::FromString(rel);
#endif
}

absl::StatusOr<FileName> FileName::Resolve() const {
#ifdef _WIN32
  constexpr DWORD access = 0;
  constexpr DWORD share = 0;
  constexpr DWORD disposition = OPEN_EXISTING;
  constexpr DWORD open_flags = FILE_FLAG_BACKUP_SEMANTICS;
  const HANDLE handle = ::CreateFileW(this->pointer(), access, share, nullptr,
                                      disposition, open_flags, nullptr);
  if (handle == INVALID_HANDLE_VALUE) {
    return WindowsStatus("CreateFileW(%#x, %#x, nullptr, %d, %#x, nullptr)",
                         access, share, disposition, open_flags);
  }
  const absl::Cleanup cleanup = [handle] {
    if (!::CloseHandle(handle)) LOG(ERROR) << WindowsStatus("CloseHandle");
  };
  std::array<wchar_t, PATHCCH_MAX_CCH> buffer;
  constexpr DWORD name_flags = FILE_NAME_NORMALIZED | VOLUME_NAME_DOS;
  const DWORD length = ::GetFinalPathNameByHandleW(
      handle, buffer.data(), DWORD{buffer.size()}, name_flags);
  if (length == 0) {
    return WindowsStatus("GetFinalPathNameByHandleW(..., ..., %#x, %#x)",
                         buffer.size(), name_flags);
  }
  if (length >= buffer.size()) {
    return absl::FailedPreconditionError(absl::StrFormat(
        "Resolved filename is too long (%d characters)", length));
  }
  std::wstring_view result(buffer.data(), length);
  // The resolved name always starts with the \\?\ prefix,
  // cf. https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfinalpathnamebyhandlew#remarks.
  // FileName doesn’t accept that, so we strip it.
  constexpr std::wstring_view prefix = L"\\\\?\\";
  if (result.substr(0, prefix.length()) != prefix) {
    return absl::FailedPreconditionError(absl::StrFormat(
        "Resolved filename %s doesn’t start with %s prefix", result, prefix));
  }
  result.remove_prefix(prefix.length());
  return FileName::FromString(result);
#else
  char* const absl_nullable result = realpath(this->pointer(), nullptr);
  if (result == nullptr) return ErrnoStatus("realpath(%#s, nullptr)", *this);
  const absl::Cleanup cleanup = [result] { std::free(result); };
  return FileName::FromString(result);
#endif
}

absl::StatusOr<std::string> ReadFile(const FileName& file) {
  std::ifstream stream(file.string(), std::ios::in | std::ios::binary);
  if (!stream.is_open() || !stream.good()) {
    return absl::UnknownError(
        absl::StrFormat("Cannot open file %s for reading", file));
  }
  stream.imbue(std::locale::classic());

  std::ostringstream buffer;
  buffer.imbue(std::locale::classic());
  buffer << stream.rdbuf();
  buffer.flush();
  if (!buffer.good() || !stream.good()) {
    return absl::UnknownError(absl::StrFormat("Cannot read file %s", file));
  }
  return buffer.str();
}

absl::Status WriteFile(const FileName& file, const std::string_view contents) {
  std::ofstream stream(file.string(),
                       std::ios::out | std::ios::trunc | std::ios::binary);
  if (!stream.is_open() || !stream.good()) {
    return absl::UnknownError(
        absl::StrFormat("Cannot open file %s for writing", file));
  }
  stream.imbue(std::locale::classic());
  const std::optional<std::streamsize> count =
      CastNumber<std::streamsize>(contents.size());
  if (!count.has_value()) {
    return absl::InvalidArgumentError(
        absl::StrFormat("Content too big (%d bytes)", contents.size()));
  }
  stream.write(contents.data(), *count);
  stream.flush();
  if (!stream.good()) {
    return absl::DataLossError(
        absl::StrFormat("Cannot write %d bytes to file %s", *count, file));
  }
  return absl::OkStatus();
}

[[nodiscard]] bool FileExists(const FileName& file) {
#ifdef _WIN32
  return ::PathFileExistsW(file.pointer()) == TRUE;
#else
  struct stat st;
  return lstat(file.pointer(), &st) == 0;
#endif
}

[[nodiscard]] bool IsNonEmptyDirectory(const FileName& directory) {
#ifdef _WIN32
  const std::wstring pattern = directory.string() + L"\\*";
  WIN32_FIND_DATAW data;
  const HANDLE handle = ::FindFirstFileW(Pointer(pattern), &data);
  if (handle == INVALID_HANDLE_VALUE) return false;
  const absl::Cleanup cleanup = [handle] {
    if (!::FindClose(handle)) LOG(ERROR) << WindowsStatus("FindClose");
  };
  do {
    const std::wstring_view name = data.cFileName;
    if (name != L"." && name != L"..") return true;
  } while (::FindNextFileW(handle, &data));
#else
  DIR* const absl_nullable handle = opendir(directory.pointer());
  if (handle == nullptr) return false;
  const absl::Cleanup cleanup = [handle] {
    if (closedir(handle) != 0) LOG(ERROR) << ErrnoStatus("closedir");
  };
  while (true) {
    const struct dirent* const absl_nullable entry = readdir(handle);
    if (entry == nullptr) break;
    const std::string_view name = entry->d_name;
    if (name != "." && name != "..") return true;
  }
#endif
  return false;
}

#undef CreateDirectory

absl::Status CreateDirectory(const FileName& name) {
#ifdef _WIN32
  const BOOL ok = ::CreateDirectoryW(name.pointer(), nullptr);
  if (!ok) return WindowsStatus("CreateDirectoryW(%#s)", name);
#else
  constexpr mode_t mode = S_IRWXU;
  const int result = mkdir(name.pointer(), mode);
  if (result != 0) return ErrnoStatus("mkdir(%#s, %#04o)", name, mode);
#endif
  return absl::OkStatus();
}

[[nodiscard]] static bool IsDirectory(const FileName& name) {
#ifdef _WIN32
  const DWORD attr = ::GetFileAttributesW(name.pointer());
  return attr != INVALID_FILE_ATTRIBUTES && (attr & FILE_ATTRIBUTE_DIRECTORY);
#else
  struct stat buffer;
  return lstat(name.pointer(), &buffer) == 0 && S_ISDIR(buffer.st_mode);
#endif
}

static absl::Status DoCreateDirectories(const FileName& name, const int depth) {
  if (depth > 100) {
    return absl::FailedPreconditionError(absl::StrFormat(
        "Potential filesystem loop when creating directory %s", name));
  }
  CHECK(name.IsAbsolute());
  if (IsDirectory(name)) return absl::OkStatus();
  const absl::StatusOr<FileName> parent = name.Parent();
  if (!parent.ok()) return parent.status();
  const absl::Status status = DoCreateDirectories(*parent, depth + 1);
  if (!status.ok()) return status;
  return CreateDirectory(name);
}

absl::Status CreateDirectories(const FileName& name) {
  const absl::StatusOr<FileName> abs = name.MakeAbsolute();
  if (!abs.ok()) return abs.status();
  return DoCreateDirectories(*abs, 0);
}

#undef RemoveDirectory

absl::Status RemoveDirectory(const FileName& name) {
#ifdef _WIN32
  const BOOL ok = ::RemoveDirectoryW(name.pointer());
  if (!ok) return WindowsStatus("RemoveDirectoryW(%#s)", name);
#else
  const int result = rmdir(name.pointer());
  if (result != 0) return ErrnoStatus("rmdir(%#s)", name);
#endif
  return absl::OkStatus();
}

absl::StatusOr<std::vector<FileName>> ListDirectory(const FileName& dir) {
  std::vector<FileName> result;
#ifdef _WIN32
  std::wstring pattern = dir.string() + L"\\*";
  WIN32_FIND_DATAW data;
  const HANDLE handle = ::FindFirstFileW(Pointer(pattern), &data);
  if (handle == INVALID_HANDLE_VALUE) {
    if (::GetLastError() != ERROR_FILE_NOT_FOUND) {
      return WindowsStatus("FindFirstFileW(%#s)", pattern);
    }
    return result;
  }
  const absl::Cleanup cleanup = [handle] {
    if (!::FindClose(handle)) LOG(ERROR) << WindowsStatus("FindClose");
  };
  do {
    const std::wstring_view name = data.cFileName;
    if (name == L"." || name == L"..") continue;
    absl::StatusOr<FileName> file = FileName::FromString(name);
    if (!file.ok()) return file.status();
    result.push_back(*std::move(file));
  } while (::FindNextFileW(handle, &data));
  if (::GetLastError() != ERROR_NO_MORE_FILES) {
    return WindowsStatus("FindNextFileW");
  }
#else
  DIR* const absl_nullable handle = opendir(dir.pointer());
  if (handle == nullptr) return ErrnoStatus("opendir(%#s)", dir);
  const absl::Cleanup cleanup = [handle] {
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
    absl::StatusOr<FileName> file = FileName::FromString(name);
    if (!file.ok()) return file.status();
    result.push_back(*std::move(file));
  }
#endif
  return result;
}

absl::Status Rename(const FileName& from, const FileName& to) {
#ifdef _WIN32
  if (!::MoveFileW(from.pointer(), to.pointer())) {
    return WindowsStatus("MoveFileW(%#s, %#s)", from, to);
  }
#else
  if (std::rename(from.pointer(), to.pointer()) != 0) {
    return ErrnoStatus("rename(%#s, %#s)", from, to);
  }
#endif
  return absl::OkStatus();
}

#ifndef _WIN32
static int Remove(const char* const absl_nonnull name, const struct stat*,
                  const int type, struct FTW* const absl_nonnull ftw) {
  switch (type) {
    case FTW_DP:
      return rmdir(name);
    case FTW_F:
    case FTW_SL:
      return unlink(name);
    default:
      LOG(ERROR) << "File " << name << " encountered at level " << ftw->level
                 << " has unsupported type " << type;
      errno = ENOTSUP;
      return -1;
  }
}
#endif

absl::Status RemoveTree(const FileName& directory) {
  const absl::StatusOr<FileName> abs = directory.MakeAbsolute();
  if (!abs.ok()) return abs.status();
#ifdef _WIN32
  SHFILEOPSTRUCTW op;
  op.hwnd = nullptr;
  op.wFunc = FO_DELETE;
  const std::wstring from = abs->string() + L'\0';
  op.pFrom = from.c_str();
  op.pTo = nullptr;
  op.fFlags = FOF_NO_UI;
  const int result = ::SHFileOperationW(&op);
  if (result != 0 || op.fAnyOperationsAborted) {
    return absl::AbortedError(
        absl::StrFormat("Removal of directory tree %s was aborted", *abs));
  }
#else
  constexpr int fd_limit = 100;
  constexpr int flags = FTW_DEPTH | FTW_MOUNT | FTW_PHYS;
  const int result = nftw(abs->pointer(), Remove, fd_limit, flags);
  if (result != 0) {
    return ErrnoStatus("nftw(%#s, ..., %d, %#x)", *abs, fd_limit, flags);
  }
#endif
  return absl::OkStatus();
}

#undef CopyFile

absl::Status CopyFile(const FileName& from, const FileName& to) {
#ifdef _WIN32
  if (!::CopyFileW(from.pointer(), to.pointer(), TRUE)) {
    return WindowsStatus("CopyFileW(%#s, %#s, TRUE)", from, to);
  }
#else
  const int from_flags = O_RDONLY | O_CLOEXEC | O_NOCTTY;
  const int from_fd = open(from.pointer(), from_flags);
  if (from_fd < 0) return ErrnoStatus("open(%#s, %#x)", from, from_flags);
  const absl::Cleanup close_from = [from_fd] {
    if (close(from_fd) != 0) LOG(ERROR) << ErrnoStatus("close(%d)", from_fd);
  };

  // Don’t allow copying directories and other irregular files.
  struct stat from_stat;
  if (fstat(from_fd, &from_stat) != 0) return ErrnoStatus("fstat(%d)", from_fd);
  if (!S_ISREG(from_stat.st_mode)) {
    return absl::FailedPreconditionError(absl::StrFormat(
        "Source file %s is irregular (mode %#04o)", from, from_stat.st_mode));
  }

  const int to_flags =
      O_WRONLY | O_CREAT | O_EXCL | O_TRUNC | O_NOFOLLOW | O_CLOEXEC | O_NOCTTY;
  const mode_t to_mode = from_stat.st_mode & (S_IRWXU | S_IRWXG | S_IRWXO);
  const int to_fd = open(to.pointer(), to_flags, to_mode);
  if (to_fd < 0) {
    return ErrnoStatus("open(%#s, %#x, %#04o)", to, to_flags, to_mode);
  }
  const absl::Cleanup close_to = [to_fd] {
    if (close(to_fd) != 0) LOG(ERROR) << ErrnoStatus("close(%d)", to_fd);
  };
#  ifdef __APPLE__
  constexpr copyfile_flags_t copy_flags =
      COPYFILE_ALL | COPYFILE_CLONE | COPYFILE_DATA_SPARSE;
  if (fcopyfile(from_fd, to_fd, nullptr, copy_flags) != 0) {
    return ErrnoStatus("fcopyfile(%d, %d, nullptr, %#x)", from_fd, to_fd,
                       copy_flags);
  }
#  else
  while (true) {
    std::array<char, 0x1000> buffer;
    const ssize_t r = read(from_fd, buffer.data(), buffer.size());
    if (r < 0) {
      return ErrnoStatus("read(%d, ..., %#x)", from_fd, buffer.size());
    }
    if (r == 0) break;
    std::string_view view(buffer.data(),
                          static_cast<std::make_unsigned_t<ssize_t>>(r));
    while (!view.empty()) {
      const ssize_t w = write(to_fd, view.data(), view.size());
      if (w < 0) return ErrnoStatus("write(%d, ..., %#x)", to_fd, view.size());
      if (w == 0) {
        // Avoid infinite loop.
        return absl::DataLossError(absl::StrFormat(
            "Cannot write %d bytes to file %s", view.size(), to));
      }
      view.remove_prefix(static_cast<std::make_unsigned_t<ssize_t>>(w));
    }
  }
  const struct timespec times[2] = {{from_stat.st_atime, 0},
                                    {from_stat.st_mtime, 0}};
  if (futimens(to_fd, times) != 0) return ErrnoStatus("futimens(%d)", to_fd);
#  endif
  if (fsync(to_fd) != 0) return ErrnoStatus("fsync(%d)", to_fd);
#endif
  return absl::OkStatus();
}

namespace {

class EnvironmentBlock final {
 private:
  using Pointer =
      std::conditional_t<kWindows, wchar_t*, const char* const absl_nullable*>;

 public:
  static absl::StatusOr<EnvironmentBlock> Current() {
#if defined _WIN32
    const absl_nullable LPWCH envp = ::GetEnvironmentStringsW();
    if (envp == nullptr) {
      // Don’t use WindowsStatus since the documentation
      // (https://learn.microsoft.com/en-us/windows/win32/api/processenv/nf-processenv-getenvironmentstringsw)
      // doesn’t say we can use GetLastError.
      return absl::ResourceExhaustedError("GetEnvironmentStringsW failed");
    }
    return EnvironmentBlock(envp);
#elif defined __APPLE__
    // See environ(7) why this is necessary.
    return EnvironmentBlock(*_NSGetEnviron());
#else
    return EnvironmentBlock(environ);
#endif
  }

  EnvironmentBlock(const EnvironmentBlock&) = delete;
  EnvironmentBlock& operator=(const EnvironmentBlock&) = delete;

  EnvironmentBlock(EnvironmentBlock&& other)
      : start_(std::exchange(other.start_, nullptr)),
        next_(std::exchange(other.next_, nullptr)) {}

  EnvironmentBlock& operator=(EnvironmentBlock&& other) {
    start_ = std::exchange(other.start_, nullptr);
    next_ = std::exchange(other.next_, nullptr);
    return *this;
  }

  ~EnvironmentBlock() noexcept {
    if (start_ == nullptr) return;
#ifdef _WIN32
    if (!::FreeEnvironmentStringsW(start_)) {
      // If this fails, we can’t really do much except logging the error.
      LOG(ERROR) << WindowsStatus("FreeEnvironmentStringsW");
    }
#endif
  }

  [[nodiscard]] bool Next(NativeStringView& element) {
    CHECK_NE(start_, nullptr);
    CHECK_NE(next_, nullptr);
#ifdef _WIN32
    element = next_;
    next_ += element.length() + 1;
    return !element.empty();
#else
    if (*next_ == nullptr) return false;
    element = *next_;
    next_++;
    return true;
#endif
  }

 private:
  explicit EnvironmentBlock(const absl_nonnull Pointer ptr)
      : start_(ABSL_DIE_IF_NULL(ptr)), next_(ptr) {}

  absl_nullable Pointer start_;
  absl_nullable Pointer next_;
};

}  // namespace

absl::StatusOr<Environment> Environment::Current() {
  absl::StatusOr<EnvironmentBlock> block = EnvironmentBlock::Current();
  if (!block.ok()) return block.status();
  Map map;
  // Skip over the first character to properly deal with the magic “per-drive
  // current directory” variables on Windows,
  // cf. https://devblogs.microsoft.com/oldnewthing/20100506-00/?p=14133.  Their
  // names start with an equals sign.
  constexpr std::size_t skip = kWindows ? 1 : 0;
  NativeStringView var;
  while (block->Next(var)) {
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
  CHECK_GT(result, 0) << WindowsStatus(
      "LCMapStringW(%d, %#x, ..., %d, nullptr, 0)", locale, flags, length);
  std::wstring buffer(result, L'\0');
  result = ::LCMapStringW(locale, flags, string.data(), length, buffer.data(),
                          result);
  CHECK_GT(result, 0) << WindowsStatus(
      "LCMapStringW(%d, %#x, ..., %d, ..., %d)", locale, flags, length,
      buffer.size());
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

absl::Status Unlink(const FileName& file) {
#ifdef _WIN32
  const BOOL result = ::DeleteFileW(file.pointer());
  if (!result) return WindowsStatus("DeleteFileW(%#s)", file);
#else
  const int result = unlink(file.pointer());
  if (result != 0) return ErrnoStatus("unlink(%#s)", file);
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
    absl::StatusOr<FileName> filename = FileName::FromString(buffer);
    if (!filename.ok()) return filename.status();
    constexpr wchar_t mode[] = L"wxbNT";
    std::FILE* const absl_nullable file = _wfopen(name, mode);
    if (file != nullptr) return TemporaryFile(*std::move(filename), file);
    status = ErrnoStatus("_wfopen(%#s, %#s)", name, mode);
    LOG(ERROR) << status;
  }
  CHECK(!status.ok());
  return status;
#else
  const char* const absl_nullable dir = std::getenv("TMPDIR");
  std::string name = absl::StrCat(dir == nullptr || *dir == '\0' ? "/tmp" : dir,
                                  "/elisp.XXXXXX");
  const int fd = mkstemp(Pointer(name));
  if (fd < 0) return ErrnoStatus("mkstemp(%#s)", name);
  const absl::StatusOr<FileName> filename = FileName::FromString(name);
  if (!filename.ok()) {
    if (unlink(Pointer(name)) != 0)
      LOG(ERROR) << ErrnoStatus("unlink(%#s)", name);
    if (close(fd) != 0) LOG(ERROR) << ErrnoStatus("close(%d)", fd);
    return filename.status();
  }
  constexpr char mode[] = "wxbe";
  std::FILE* const absl_nullable file = fdopen(fd, mode);
  if (file == nullptr) {
    const absl::Status status = ErrnoStatus("fdopen(%d, %#s)", fd, mode);
    if (close(fd) != 0) LOG(ERROR) << ErrnoStatus("close(%d)", fd);
    const absl::Status unlink_status = Unlink(*filename);
    if (!unlink_status.ok()) LOG(ERROR) << unlink_status;
    return status;
  }
  return TemporaryFile(*std::move(filename), file);
#endif
}

TemporaryFile::~TemporaryFile() noexcept {
  if (!name_.has_value()) return;
  CHECK(file_ != nullptr);
  if (std::fclose(file_) != 0) LOG(FATAL) << ErrnoStatus("fclose");
  const absl::Status unlink_status = Unlink(*name_);
  if (!unlink_status.ok()) LOG(ERROR) << unlink_status;
}

absl::Status TemporaryFile::Write(const std::string_view contents) {
  if (!contents.empty()) {
    constexpr std::size_t size = 1;
    const std::size_t count = contents.length();
    const std::size_t written =
        std::fwrite(contents.data(), size, count, file_);
    if (written != contents.size()) {
      const absl::Status status =
          ErrnoStatus("fwrite(..., %d, %d)", size, count);
      LOG(ERROR) << status << "; only " << written << " bytes of " << count
                 << " written";
      return status;
    }
  }
  if (std::fflush(file_) != 0) return ErrnoStatus("fflush");
  return absl::OkStatus();
}

absl::StatusOr<FileName> CreateTemporaryDirectory() {
#ifdef _WIN32
  absl::Status status;
  for (int i = 0; i < 10; i++) {
    std::array<wchar_t, L_tmpnam + 1> buffer;
    const wchar_t* absl_nullable name = _wtmpnam(buffer.data());
    if (name == nullptr) {
      return absl::UnavailableError("Cannot create temporary name");
    }
    absl::StatusOr<FileName> result = FileName::FromString(name);
    if (!result.ok()) return result.status();
    status = CreateDirectory(*result);
    if (status.ok()) return *std::move(result);
    LOG(ERROR) << status;
  }
  CHECK(!status.ok());
  return status;
#else
  const char* const absl_nullable dir = std::getenv("TMPDIR");
  std::string buffer = absl::StrCat(
      dir == nullptr || *dir == '\0' ? "/tmp" : dir, "/elisp.XXXXXX");
  char* const absl_nullable name = mkdtemp(Pointer(buffer));
  if (name == nullptr) return ErrnoStatus("mkdtemp(%#s)", buffer);
  const absl::StatusOr<FileName> result = FileName::FromString(name);
  if (!result.ok()) {
    if (rmdir(name) != 0) LOG(ERROR) << ErrnoStatus("rmdir(%#s)", name);
    return result.status();
  }
  return *std::move(result);
#endif
}

#undef SearchPath

absl::StatusOr<FileName> SearchPath(const FileName& program) {
#ifdef _WIN32
  constexpr LPCWSTR extension = L".exe";
  std::array<wchar_t, PATHCCH_MAX_CCH> buffer;
  const DWORD length =
      ::SearchPathW(nullptr, program.pointer(), extension, DWORD{buffer.size()},
                    buffer.data(), nullptr);
  if (length == 0) {
    return WindowsStatus("SearchPathW(nullptr, %#s, %#s, %#x, ..., nullptr)",
                         program, extension, buffer.size());
  }
  if (length >= buffer.size()) {
    return absl::FailedPreconditionError(
        absl::StrFormat("Program filename too long (%d characters)", length));
  }
  return FileName::FromString(std::wstring_view(buffer.data(), length));
#else
  // See the description of PATH at
  // https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap08.html#tag_08_03.
  const std::string& string = program.string();
  if (string.find('/') != string.npos) return program;
  const char* const absl_nullable path = std::getenv("PATH");
  if (path == nullptr) {
    return absl::NotFoundError("PATH environment variable not set");
  }
  for (const std::string_view dir : absl::StrSplit(path, ':')) {
    std::string file(dir.empty() ? "." : dir);
    if (file.back() != '/') file += '/';
    file += string;
    if (access(Pointer(file), X_OK) == 0) return FileName::FromString(file);
  }
  return absl::NotFoundError(
      absl::StrFormat("Program %s not found in PATH %s", program, path));
#endif
}

static void FlushEverything() {
  std::cout.flush();
  std::wcout.flush();
  std::cerr.flush();
  std::wcerr.flush();
  if (std::fflush(nullptr) != 0) LOG(ERROR) << ErrnoStatus("fflush(nullptr)");
}

absl::StatusOr<int> RunProcess(const FileName& program,
                               const absl::Span<const NativeString> args,
                               const Environment& env,
                               const ProcessOptions& options) {
  for (const NativeString& arg : args) {
    if (ContainsNull(arg)) {
      return absl::InvalidArgumentError(
          absl::StrFormat("Argument %s contains null character", arg));
    }
  }
  const NativeString& string = program.string();
  if (string.find(kSeparator) == string.npos) {
    return absl::InvalidArgumentError(absl::StrFormat(
        "Program name %s doesn’t contain a directory separator character",
        program));
  }
  const absl::StatusOr<FileName> abs_program = program.MakeAbsolute();
  if (!abs_program.ok()) return abs_program.status();
  std::vector<NativeString> args_vec = {program.string()};
  args_vec.insert(args_vec.end(), args.cbegin(), args.cend());
  std::vector<NativeString> final_env;
  for (const auto& [key, value] : env) {
    final_env.push_back(key + RULES_ELISP_NATIVE_LITERAL('=') + value);
  }
  // Sort entries for hermeticity.
  absl::c_sort(final_env);
  const bool has_deadline = options.deadline < absl::InfiniteFuture();
  FlushEverything();
  const absl::Cleanup flush = FlushEverything;
#ifdef _WIN32
  absl::StatusOr<std::wstring> command_line = BuildCommandLine(args_vec);
  if (!command_line.ok()) return command_line.status();
  const BOOL inherit_handles = options.output_file.has_value() ? TRUE : FALSE;
  const DWORD flags = CREATE_UNICODE_ENVIRONMENT |
                      (has_deadline ? CREATE_NEW_PROCESS_GROUP : 0);
  absl::StatusOr<std::wstring> envp = BuildEnvironmentBlock(final_env);
  if (!envp.ok()) return envp.status();
  const absl_nullable LPCWSTR dirp =
      options.directory.has_value() ? options.directory->pointer() : nullptr;
  STARTUPINFOW startup_info;
  startup_info.cb = sizeof startup_info;
  startup_info.lpReserved = nullptr;
  startup_info.lpDesktop = nullptr;
  startup_info.lpTitle = nullptr;
  startup_info.dwFlags =
      options.output_file.has_value() ? STARTF_USESTDHANDLES : 0;
  startup_info.cbReserved2 = 0;
  startup_info.lpReserved2 = nullptr;
  if (options.output_file.has_value()) {
    startup_info.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    if (startup_info.hStdInput == INVALID_HANDLE_VALUE) {
      return WindowsStatus("GetStdHandle(%d)", STD_INPUT_HANDLE);
    }
    constexpr DWORD access = GENERIC_WRITE;
    constexpr DWORD share = FILE_SHARE_READ;
    SECURITY_ATTRIBUTES security;
    security.nLength = sizeof security;
    security.lpSecurityDescriptor = nullptr;
    security.bInheritHandle = TRUE;
    constexpr DWORD disposition = CREATE_NEW;
    constexpr DWORD attributes = FILE_ATTRIBUTE_NORMAL;
    startup_info.hStdOutput =
        ::CreateFileW(options.output_file->pointer(), access, share, &security,
                      disposition, attributes, nullptr);
    if (startup_info.hStdOutput == INVALID_HANDLE_VALUE) {
      return WindowsStatus("CreateFileW(%#s, %#x, %#x, ..., %d, %#x, nullptr)",
                           *options.output_file, access, share, disposition,
                           attributes);
    }
    startup_info.hStdError = startup_info.hStdOutput;
  }
  const absl::Cleanup close_output = [&startup_info] {
    if (startup_info.dwFlags & STARTF_USESTDHANDLES) {
      if (!::CloseHandle(startup_info.hStdOutput)) {
        LOG(ERROR) << WindowsStatus("CloseHandle");
      }
    }
  };
  PROCESS_INFORMATION process_info;
  if (!::CreateProcessW(abs_program->pointer(), Pointer(*command_line), nullptr,
                        nullptr, inherit_handles, flags, envp->data(), dirp,
                        &startup_info, &process_info)) {
    return WindowsStatus(
        "CreateProcessW(%#s, %#s, nullptr, nullptr, %d, %#x, ..., %#s)",
        *abs_program, *command_line, inherit_handles, flags, dirp);
  }
  if (!::CloseHandle(process_info.hThread)) {
    LOG(ERROR) << WindowsStatus("CloseHandle");
  }
  const absl::Cleanup close_handle = [&process_info] {
    if (!::CloseHandle(process_info.hProcess)) {
      LOG(ERROR) << WindowsStatus("CloseHandle");
    }
  };
  const DWORD timeout_ms =
      has_deadline
          ? static_cast<DWORD>(std::clamp(
                absl::ToInt64Milliseconds(options.deadline - absl::Now()),
                std::int64_t{0}, std::int64_t{MAXDWORD}))
          : INFINITE;
  switch (::WaitForSingleObject(process_info.hProcess, timeout_ms)) {
    case WAIT_OBJECT_0:
      break;
    case WAIT_TIMEOUT:
      LOG(WARNING) << "Process timed out, sending CTRL + BREAK";
      if (!::GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT,
                                      process_info.dwProcessId)) {
        LOG(ERROR) << WindowsStatus("GenerateConsoleCtrlEvent(%d, %d)",
                                    CTRL_BREAK_EVENT, process_info.dwProcessId);
      }
      return absl::DeadlineExceededError(absl::StrFormat(
          "Deadline %v exceeded waiting for process (timeout %v)",
          options.deadline, absl::Milliseconds(timeout_ms)));
    default:
      return WindowsStatus("WaitForSingleObject(..., %d)", timeout_ms);
  }
  DWORD code;
  if (!::GetExitCodeProcess(process_info.hProcess, &code)) {
    return WindowsStatus("GetExitCodeProcess");
  }
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
        absl::StrFormat("Finite deadline %v unsupported", options.deadline));
  }
  posix_spawn_file_actions_t actions;
  if (posix_spawn_file_actions_init(&actions) != 0) {
    return ErrnoStatus("posix_spawn_file_actions_init");
  }
  const absl::Cleanup cleanup = [&actions] {
    if (posix_spawn_file_actions_destroy(&actions) != 0) {
      LOG(ERROR) << ErrnoStatus("posix_spawn_file_actions_destroy");
    }
  };
  if (options.output_file.has_value()) {
    constexpr int oflag = O_WRONLY | O_CREAT | O_TRUNC | O_NOCTTY;
    constexpr mode_t mode = S_IRUSR | S_IWUSR;
    if (posix_spawn_file_actions_addopen(&actions, STDOUT_FILENO,
                                         options.output_file->pointer(), oflag,
                                         mode) != 0) {
      return ErrnoStatus(
          "posix_spawn_file_actions_addopen(..., %d, %#s, %#x, %#04o)",
          STDOUT_FILENO, *options.output_file, oflag, mode);
    }
    if (posix_spawn_file_actions_adddup2(&actions, STDOUT_FILENO,
                                         STDERR_FILENO) != 0) {
      return ErrnoStatus("posix_spawn_file_actions_adddup2(..., %d, %d)",
                         STDOUT_FILENO, STDERR_FILENO);
    }
  }
  if (options.directory.has_value()) {
    // TODO: Switch to posix_spawn_file_actions_addchdir once that’s widely
    // available.
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wdeprecated-declarations"
    const int result = posix_spawn_file_actions_addchdir_np(
        &actions, options.directory->pointer());
#  pragma GCC diagnostic pop
    if (result != 0) {
      return ErrnoStatus("posix_spawn_file_actions_addchdir_np(..., %#s)",
                         *options.directory);
    }
  }
  const std::vector<char* absl_nullable> argv = Pointers(args_vec);
  const std::vector<char* absl_nullable> envp = Pointers(final_env);
  pid_t pid;
  const int error = posix_spawn(&pid, abs_program->pointer(), &actions, nullptr,
                                argv.data(), envp.data());
  if (error != 0) {
    return ErrorStatus(std::error_code(error, std::system_category()),
                       "posix_spawn(..., %#s)", *abs_program);
  }
  int wstatus;
  const pid_t status = waitpid(pid, &wstatus, 0);
  if (status != pid) return ErrnoStatus("waitpid(%d, ..., 0)", pid);
  return WIFEXITED(wstatus) ? WEXITSTATUS(wstatus) : 0xFF;
#endif
}

absl::StatusOr<DosDevice> DosDevice::Create(
    [[maybe_unused]] const FileName& target) {
#ifdef _WIN32
  const DWORD drives = ::GetLogicalDrives();
  if (drives == 0) return WindowsStatus("GetLogicalDrives");
  constexpr wchar_t first = L'D';
  constexpr wchar_t last = L'Z';
  constexpr unsigned int count{last - first + 1};
  std::array<wchar_t, count> letters;
  auto end = letters.begin();
  for (wchar_t letter = first; letter <= last; ++letter) {
    static_assert(sizeof(DWORD) * CHAR_BIT > count);
    const DWORD bit{1U << (letter - L'A')};
    if ((drives & bit) == 0) *end++ = letter;
  }
  absl::BitGen gen;
  std::shuffle(letters.begin(), end, gen);
  for (auto it = letters.begin(); it != end; ++it) {
    constexpr DWORD flags = DDD_NO_BROADCAST_SYSTEM;
    const wchar_t name[] = {*it, L':', L'\0'};
    if (!::DefineDosDeviceW(flags, name, target.pointer())) {
      return WindowsStatus("DefineDosDeviceW(%#x, %#s, %#s)", flags, name,
                           target);
    }
    return DosDevice(name, target);
  }
  return absl::ResourceExhaustedError("no drive letters available");
#else
  return absl::UnimplementedError("this system doesn’t support DOS devices");
#endif
}

DosDevice::~DosDevice() noexcept {
#ifdef _WIN32
  if (name_.empty()) return;
  CHECK(target_.has_value());
  constexpr DWORD flags = DDD_REMOVE_DEFINITION | DDD_EXACT_MATCH_ON_REMOVE |
                          DDD_NO_BROADCAST_SYSTEM;
  if (!::DefineDosDeviceW(flags, Pointer(name_), target_->pointer())) {
    LOG(ERROR) << WindowsStatus("DefineDosDeviceW(%#x, %#s, %#s)", flags, name_,
                                *target_);
  }
#endif
}

}  // namespace rules_elisp
