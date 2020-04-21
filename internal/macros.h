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

#ifndef PHST_RULES_ELISP_INTERNAL_MACROS_H
#define PHST_RULES_ELISP_INTERNAL_MACROS_H

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#include "absl/status/status.h"
#pragma GCC diagnostic pop

#define RETURN_IF_ERROR(expr)          \
  do {                                 \
    const auto _status = (expr);       \
    if (!_status.ok()) return _status; \
  } while (false)

#define ASSIGN_OR_RETURN(lhs, rhs) \
  ASSIGN_OR_RETURN_1(lhs, (rhs), __COUNTER__)

#define ASSIGN_OR_RETURN_1(lhs, rhs, counter) \
  ASSIGN_OR_RETURN_2(lhs, (rhs), PHST_RULES_ELISP_CONCAT(_status_or_, counter))

#define ASSIGN_OR_RETURN_2(lhs, rhs, var) \
  auto var = (rhs);                       \
  if (!var.ok()) return var.status();     \
  lhs = std::move(var).value()

#define PHST_RULES_ELISP_CONCAT(a, b) a ## b

#endif
