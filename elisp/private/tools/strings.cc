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

#include <cstdint>
#include <iomanip>
#include <ios>
#include <locale>
#include <sstream>
#include <string>
#include <string_view>
#include <type_traits>

#include "absl/algorithm/container.h"
#include "absl/status/status.h"
#include "absl/strings/ascii.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"

#include "elisp/private/tools/numeric.h"

namespace rules_elisp {

namespace {

template <typename Char>
std::string DoQuote(const std::basic_string_view<Char> string) {
  std::basic_ostringstream<Char> stream;
  stream.exceptions(std::ios::badbit | std::ios::failbit | std::ios::eofbit);
  stream.imbue(std::locale::classic());
  stream << std::quoted(string) << std::flush;
  return absl::StrFormat("%s", stream.str());
}

}  // namespace

std::string Quote(const std::string_view string) { return DoQuote(string); }
std::string Quote(const std::wstring_view string) { return DoQuote(string); }

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

}  // namespace

absl::Status CheckAscii(const std::string_view string) {
  return DoCheckAscii(string);
}

absl::Status CheckAscii(const std::wstring_view string) {
  return DoCheckAscii(string);
}

std::string PercentEncode(std::string_view string) {
  std::string result;
  while (!string.empty()) {
    const std::string_view::const_iterator it =
        absl::c_find_if_not(string, [](const char ch) {
          if (ch <= 0 || ch == '%') return false;
          const unsigned char u = static_cast<unsigned char>(ch);
          return u <= kMaxAscii && absl::ascii_isprint(u);
        });
    result.append(string.cbegin(), it);
    string.remove_prefix(
        CastNumber<std::string_view::size_type>(it - string.cbegin()).value());
    if (!string.empty()) {
      const std::uint8_t ch{static_cast<unsigned char>(string.front())};
      absl::StrAppend(&result, "%", absl::Hex(ch, absl::kZeroPad2));
      string.remove_prefix(1);
    }
  }
  return result;
}

}  // namespace rules_elisp
