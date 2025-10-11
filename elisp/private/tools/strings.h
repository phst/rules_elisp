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

#ifndef ELISP_PRIVATE_TOOLS_STRINGS_H_
#define ELISP_PRIVATE_TOOLS_STRINGS_H_

#include <optional>
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

std::string Escape(NativeStringView string);

inline constexpr unsigned int kMaxASCII{0x7F};

template <typename String>
absl::Status CheckASCII(const String& string) {
  using Traits = typename String::traits_type;
  using Char = typename Traits::char_type;
  const auto it = absl::c_find_if(string, [](const Char ch) {
    return Traits::lt(ch, Char{0}) || Traits::lt(Char{kMaxASCII}, ch);
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
  static_assert(InRange<ToChar>(kMaxASCII),
                "destination character type too small");
  const absl::Status status = CheckASCII(string);
  if (!status.ok()) return status;
  ToString ret;
  ret.reserve(string.length());
  for (FromChar ch : string) {
    const std::optional<ToChar> to = CastNumberOpt<ToChar>(ch);
    CHECK(to.has_value()) << "character " << ch << " too large";
    ret.push_back(*to);
  }
  return ret;
}

inline absl::StatusOr<std::string> ToNarrow(const NativeStringView string) {
  return ConvertString<std::string>(string);
}

inline absl::StatusOr<NativeString> ToNative(const std::string_view string) {
  return ConvertString<NativeString>(string);
}

}  // namespace rules_elisp

#endif  // ELISP_PRIVATE_TOOLS_STRINGS_H_
