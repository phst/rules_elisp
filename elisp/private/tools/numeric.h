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

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"

namespace rules_elisp {

template <typename To, typename From>
[[nodiscard]] constexpr bool Overflow(const From n) {
  static_assert(std::is_integral_v<To>);
  static_assert(std::is_integral_v<From>);
  using ToLimits = std::numeric_limits<To>;
  using FromLimits = std::numeric_limits<From>;
  if constexpr (ToLimits::is_signed == FromLimits::is_signed) {
    return n < ToLimits::min() || n > ToLimits::max();
  }
  if constexpr (ToLimits::is_signed && !FromLimits::is_signed) {
    return n > std::make_unsigned_t<To>{ToLimits::max()};
  }
  if constexpr (!ToLimits::is_signed && FromLimits::is_signed) {
    return n < 0 ||
           static_cast<std::make_unsigned_t<From>>(n) > ToLimits::max();
  }
}

template <typename To, typename From>
std::optional<To> CastNumberOpt(const From n) {
  return Overflow<To>(n) ? std::optional<To>() : static_cast<To>(n);
}

template <typename To, typename From>
absl::StatusOr<To> CastNumber(const From n) {
  using Limits = std::numeric_limits<To>;
  const std::optional<To> ret = CastNumberOpt<To>(n);
  if (!ret) {
    return absl::OutOfRangeError(absl::StrCat("Number ", n, " out of range [",
                                              Limits::min(), ", ",
                                              Limits::max(), "]"));
  }
  return *ret;
}

}  // namespace rules_elisp

#endif  // ELISP_PRIVATE_TOOLS_CAST_H_
