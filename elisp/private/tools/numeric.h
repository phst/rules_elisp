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

#ifndef ELISP_PRIVATE_TOOLS_CAST_H_
#define ELISP_PRIVATE_TOOLS_CAST_H_

#include <limits>
#include <optional>
#include <type_traits>

namespace rules_elisp {

template <typename R, typename T>
[[nodiscard]] constexpr bool InRange(const T n) {
  static_assert(std::is_integral_v<R>);
  static_assert(std::is_integral_v<T>);
  using RL = std::numeric_limits<R>;
  using TL = std::numeric_limits<T>;
  if constexpr (RL::is_signed == TL::is_signed) {
    return n >= RL::min() && n <= RL::max();
  }
  if constexpr (RL::is_signed && !TL::is_signed) {
    return n <= std::make_unsigned_t<R>{RL::max()};
  }
  if constexpr (!RL::is_signed && TL::is_signed) {
    return n >= 0 && static_cast<std::make_unsigned_t<T>>(n) <= RL::max();
  }
}

template <typename To, typename From>
std::optional<To> CastNumberOpt(const From n) {
  return InRange<To>(n) ? static_cast<To>(n) : std::optional<To>();
}

}  // namespace rules_elisp

#endif  // ELISP_PRIVATE_TOOLS_CAST_H_
