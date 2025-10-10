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

#ifndef ELISP_PRIVATE_TOOLS_SYSTEM_H_
#define ELISP_PRIVATE_TOOLS_SYSTEM_H_

#include <string_view>
#include <system_error>
#include <tuple>
#include <utility>

#include "absl/status/status.h"
#include "absl/strings/str_join.h"

namespace rules_elisp {

absl::Status MakeErrorStatus(const std::error_code& code,
                             std::string_view function, std::string_view args);

template <typename... Ts>
absl::Status ErrorStatus(const std::error_code& code,
                         const std::string_view function, Ts&&... args) {
  return MakeErrorStatus(code, function,
                         absl::StrJoin(std::forward_as_tuple(args...), ", "));
}

#ifdef _WIN32
[[nodiscard]] std::error_code WindowsError();

template <typename... Ts>
absl::Status WindowsStatus(const std::string_view function, Ts&&... args) {
  const std::error_code code = WindowsError();
  return ErrorStatus(code, function, std::forward<Ts>(args)...);
}
#else
[[nodiscard]] std::error_code ErrnoError();

template <typename... Ts>
absl::Status ErrnoStatus(const std::string_view function, Ts&&... args) {
  const std::error_code code = ErrnoError();
  return ErrorStatus(code, function, std::forward<Ts>(args)...);
}
#endif

}  // namespace rules_elisp

#endif  // ELISP_PRIVATE_TOOLS_SYSTEM_H_
