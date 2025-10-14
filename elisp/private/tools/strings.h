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

#include <string>
#include <string_view>

#include "absl/status/status.h"
#include "absl/status/statusor.h"

#include "elisp/private/tools/platform.h"

namespace rules_elisp {

std::string Quote(std::string_view string);
std::string Quote(std::wstring_view string);

absl::Status CheckAscii(const std::string_view string);
absl::Status CheckAscii(const std::wstring_view string);

absl::StatusOr<std::string> ToNarrow(NativeStringView string);
absl::StatusOr<NativeString> ToNative(std::string_view string);

}  // namespace rules_elisp

#endif  // ELISP_PRIVATE_TOOLS_STRINGS_H_
