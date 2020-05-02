// Copyright 2020 Google LLC
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

#ifndef PHST_RULES_ELISP_INTERNAL_STR_H
#define PHST_RULES_ELISP_INTERNAL_STR_H

#include <cstdlib>
#include <iostream>
#include <string>

namespace phst_rules_elisp {

inline const char* Pointer(const std::string& s) {
  if (s.find('\0') != s.npos) {
    std::clog << s << " contains null character" << std::endl;
    std::abort();
  }
  return s.c_str();
}

// Don’t allow passing a temporary.
inline void Pointer(const std::string&&) = delete;

}  // phst_rules_elisp

#endif
