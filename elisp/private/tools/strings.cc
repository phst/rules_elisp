// Copyright 2020-2025 Google LLC
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

#include "elisp/private/tools/strings.h"

#include <iomanip>
#include <ios>
#include <locale>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <type_traits>

#include "absl/algorithm/container.h"
#include "absl/log/check.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"

#include "elisp/private/tools/numeric.h"
#include "elisp/private/tools/platform.h"

namespace rules_elisp {

namespace {

template <typename Char>
std::string DoQuote(const std::basic_string_view<Char> string) {
  std::basic_ostringstream<Char> stream;
  stream.exceptions(std::ios::badbit | std::ios::failbit | std::ios::eofbit);
  stream.imbue(std::locale::classic());
  stream << std::quoted(string);
  return absl::StrFormat("%s", stream.str());
}

}  // namespace

std::string Quote(const std::string_view string) { return DoQuote(string); }

std::string Quote(const std::wstring_view string) {
  return absl::StrCat("L", DoQuote(string));
}

namespace {

inline constexpr unsigned int kMaxAscii{0x7F};

template <typename Char>
absl::Status DoCheckAscii(const std::basic_string_view<Char> string) {
  using Traits = typename std::basic_string_view<Char>::traits_type;
  const auto it = absl::c_find_if(string, [](const Char ch) {
    return Traits::lt(ch, Char{0}) || Traits::lt(Char{kMaxAscii}, ch);
  });
  if (it != string.end()) {
    const auto val = static_cast<std::make_unsigned_t<Char>>(*it);
    return absl::InvalidArgumentError(
        absl::StrCat("non-ASCII character U+", absl::Hex(val, absl::kZeroPad4),
                     " in string"));
  }
  return absl::OkStatus();
}

template <typename Char>
absl::Status DoCheckNull(const std::basic_string_view<Char> string) {
  if (const auto i = string.find(Char{'\0'}); i != string.npos) {
    return absl::InvalidArgumentError(
        absl::StrFormat("String %s contains embedded null character", string));
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
  static_assert(InRange<ToChar>(kMaxAscii),
                "destination character type too small");
  const absl::Status status = CheckAscii(string);
  if (!status.ok()) return status;
  ToString ret;
  ret.reserve(string.length());
  for (FromChar ch : string) {
    const std::optional<ToChar> to = CastNumber<ToChar>(ch);
    CHECK(to.has_value()) << "character " << ch << " too large";
    ret.push_back(*to);
  }
  return ret;
}

}  // namespace

absl::Status CheckAscii(const std::string_view string) {
  return DoCheckAscii(string);
}

absl::Status CheckAscii(const std::wstring_view string) {
  return DoCheckAscii(string);
}

absl::Status CheckNull(const std::string_view string) {
  return DoCheckNull(string);
}

absl::Status CheckNull(const std::wstring_view string) {
  return DoCheckNull(string);
}

absl::StatusOr<std::string> ToNarrow(const NativeStringView string) {
  return ConvertString<std::string>(string);
}

absl::StatusOr<NativeString> ToNative(const std::string_view string) {
  return ConvertString<NativeString>(string);
}

}  // namespace rules_elisp
