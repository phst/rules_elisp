// Copyright 2020, 2021, 2022, 2023, 2024 Google LLC
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

#ifndef RULES_ELISP_ELISP_TEST_H
#define RULES_ELISP_ELISP_TEST_H

#if !defined __cplusplus || __cplusplus < 201703L
#  error this file requires at least C++17
#endif

#include <initializer_list>

#include "elisp/platform.h"  // IWYU pragma: export

namespace rules_elisp {

[[nodiscard]] int RunTest(std::initializer_list<NativeStringView> prefix,
                          int argc, const NativeChar* const* argv);

}  // namespace rules_elisp

#endif
