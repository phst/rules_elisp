// Copyright 2021, 2023, 2024 Google LLC
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

#ifndef RULES_ELISP_ELISP_PLATFORM_H
#define RULES_ELISP_ELISP_PLATFORM_H

#if !defined __cplusplus || __cplusplus < 201703L
#  error this file requires at least C++17
#endif

#include <string>
#include <string_view>

namespace rules_elisp {

#ifdef _WIN32
using NativeChar = wchar_t;
#  define RULES_ELISP_NATIVE_LITERAL(literal) L##literal
#else
using NativeChar = char;
#  define RULES_ELISP_NATIVE_LITERAL(literal) literal
#endif

using NativeString = std::basic_string<NativeChar>;
using NativeStringView = std::basic_string_view<NativeChar>;

}  // namespace rules_elisp

#endif
