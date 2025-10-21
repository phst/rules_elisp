// Copyright 2021, 2023-2025 Google LLC
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

#ifndef ELISP_PRIVATE_TOOLS_PLATFORM_H_
#define ELISP_PRIVATE_TOOLS_PLATFORM_H_

#if !defined __cplusplus || __cplusplus < 201703L
#  error this file requires at least C++17
#endif

#include <string>
#include <string_view>
#include <type_traits>
#include <vector>

namespace rules_elisp {

constexpr inline bool kWindows =
#ifdef _WIN32
    true
#else
    false
#endif
    ;

#ifdef _WIN32
#  define RULES_ELISP_NATIVE_LITERAL(literal) L##literal
#  define RULES_ELISP_MAIN wmain
#else
#  define RULES_ELISP_NATIVE_LITERAL(literal) literal
#  define RULES_ELISP_MAIN main
#endif

using NativeChar = std::conditional_t<kWindows, wchar_t, char>;
using NativeString = std::basic_string<NativeChar>;
using NativeStringView = std::basic_string_view<NativeChar>;

inline constexpr NativeChar kSeparator = kWindows
                                             ? RULES_ELISP_NATIVE_LITERAL('\\')
                                             : RULES_ELISP_NATIVE_LITERAL('/');

enum class Mode { kSource, kRelease };
enum class ToolchainMode { kDirect, kWrap };

struct Options final {
  // Options for both binaries and tests.
  NativeString wrapper;
  ToolchainMode mode = ToolchainMode::kDirect;
  std::vector<NativeString> tags;
  std::vector<NativeString> load_path;
  std::vector<NativeString> load_files;
  std::vector<NativeString> data_files;

  // Options only relevant for binaries.
  bool interactive = false;
  std::vector<int> input_args;
  std::vector<int> output_args;

  // Options only relevant for tests.
  std::vector<NativeString> skip_tests;
  std::vector<NativeString> skip_tags;
  bool module_assertions = false;
};

}  // namespace rules_elisp

#endif  // ELISP_PRIVATE_TOOLS_PLATFORM_H_
