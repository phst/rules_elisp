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

#ifndef PHST_RULES_ELISP_INTERNAL_INT_H
#define PHST_RULES_ELISP_INTERNAL_INT_H

#include <limits>
#include <type_traits>

namespace phst_rules_elisp {

template <typename T>
constexpr typename std::enable_if<std::is_integral<T>::value &&
                                      std::is_signed<T>::value,
                                  typename std::make_unsigned<T>::type>::type
unsigned_max() noexcept {
  static_assert(std::numeric_limits<T>::max() >= 0, "type maximum is negative");
  return static_cast<typename std::make_unsigned<T>::type>(
      std::numeric_limits<T>::max());
}

template <typename T>
constexpr typename std::enable_if<
    std::is_integral<T>::value && std::is_unsigned<T>::value, T>::type
unsigned_max() noexcept {
  return std::numeric_limits<T>::max();
}

template <typename T>
constexpr typename std::enable_if<!std::is_integral<T>::value>::type
unsigned_max() noexcept = delete;

}  // phst_rules_elisp

#endif
