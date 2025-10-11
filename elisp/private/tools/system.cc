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
#  include <unistd.h>
#endif

#ifdef __APPLE__
#  include <crt_externs.h>  // for _NSGetEnviron
#endif

#include <cerrno>
#include <cstddef>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <system_error>
#include <type_traits>

#include "absl/base/attributes.h"
#include "absl/base/nullability.h"
#include "absl/hash/hash.h"
#include "absl/log/check.h"
#include "absl/log/die_if_null.h"
#include "absl/log/log.h"
#include "absl/status/status.h"
#include "absl/strings/str_cat.h"

#include "elisp/private/tools/numeric.h"
#include "elisp/private/tools/strings.h"

namespace rules_elisp {

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

#ifdef _WIN32
[[nodiscard]] std::error_code WindowsError() {
  const DWORD code = ::GetLastError();
  const std::optional<int> i = CastNumberOpt<int>(code);
  return i.has_value() ? std::error_code(*i, std::system_category())
                       : std::make_error_code(std::errc::value_too_large);
}
#else
[[nodiscard]] std::error_code ErrnoError() {
  const int code = errno;
  return std::error_code(code, std::system_category());
}
#endif

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

std::size_t CaseInsensitiveHash::operator()(
    const std::wstring_view string) const {
  return absl::HashOf(ToUpper(string));
}

bool CaseInsensitiveEqual::operator()(const std::wstring_view a,
                                      const std::wstring_view b) const {
  return ToUpper(a) == ToUpper(b);
}
#endif

}  // namespace rules_elisp
