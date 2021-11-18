// Copyright 2021 Google LLC
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

#ifndef PHST_RULES_ELISP_ELISP_PLATFORM_H
#define PHST_RULES_ELISP_ELISP_PLATFORM_H

#include <string>

namespace phst_rules_elisp {

#if defined _WIN32 || defined _WIN64
#define PHST_RULES_ELISP_WINDOWS
using NativeChar = wchar_t;
#define PHST_RULES_ELISP_NATIVE_LITERAL(literal) L##literal
#define PHST_RULES_ELISP_MAIN wmain
#else
#define PHST_RULES_ELISP_NATIVE_LITERAL(literal) literal
#define PHST_RULES_ELISP_MAIN main
using NativeChar = char;
#endif

using NativeString = std::basic_string<NativeChar>;

}  // namespace phst_rules_elisp

#endif
