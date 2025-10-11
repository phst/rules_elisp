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

#include <optional>
#include <string>

#include "absl/strings/ascii.h"
#include "absl/strings/str_cat.h"

#include "elisp/private/tools/numeric.h"
#include "elisp/private/tools/platform.h"

namespace rules_elisp {

std::string Escape(const NativeStringView string) {
#ifdef _WIN32
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
#else
  return std::string(string);
#endif
}

}  // namespace rules_elisp
