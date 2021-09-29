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

#ifndef PHST_RULES_ELISP_ELISP_FILE_H
#define PHST_RULES_ELISP_ELISP_FILE_H

#include <initializer_list>
#include <string>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#include "absl/base/attributes.h"
#include "absl/base/casts.h"
#include "absl/status/statusor.h"
#include "absl/strings/string_view.h"
#pragma GCC diagnostic pop

namespace phst_rules_elisp {

constexpr bool IsAbsolute(absl::string_view name) noexcept {
  return !name.empty() && name.front() == '/';
}

std::string JoinPathImpl(std::initializer_list<absl::string_view> pieces);

template <typename... Ts>
std::string JoinPath(Ts&&... pieces) {
  static_assert(sizeof...(pieces) >= 1, "need at least one piece to join");
  return JoinPathImpl({absl::implicit_cast<absl::string_view>(pieces)...});
}

absl::StatusOr<std::string> MakeAbsolute(absl::string_view name);

ABSL_MUST_USE_RESULT bool FileExists(const std::string& name) noexcept;

}  // phst_rules_elisp

#endif
