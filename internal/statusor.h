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

#ifndef PHST_RULES_ELISP_INTERNAL_STATUSOR_H
#define PHST_RULES_ELISP_INTERNAL_STATUSOR_H

#include <cstdlib>
#include <iostream>
#include <type_traits>
#include <utility>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#include "absl/base/attributes.h"
#include "absl/base/macros.h"
#include "absl/status/status.h"
#include "absl/types/variant.h"
#pragma GCC diagnostic pop

namespace phst_rules_elisp {

template <typename T>
class ABSL_MUST_USE_RESULT StatusOr {
 public:
  static_assert(!std::is_convertible<T, absl::Status>::value,
                "StatusOr type may not be absl::Status");

  StatusOr(absl::Status status)
#ifdef ABSL_BAD_CALL_IF
      ABSL_BAD_CALL_IF(status.ok(), "can’t initialize status_or with OK status")
#endif
  {
    if (status.ok()) {
      std::clog << "can’t initialize status_or with OK status" << std::endl;
      std::abort();
    }
    data_ = std::move(status);
  }

  StatusOr(T value) : data_(std::move(value)) {}

  absl::Status status() const {
    return this->ok() ? absl::OkStatus() : absl::get<absl::Status>(data_);
  }

  bool ok() const { return absl::holds_alternative<T>(data_); }
  T& value()& { return absl::get<T>(data_); }
  const T& value() const& { return absl::get<T>(data_); }
  T&& value()&& { return absl::get<T>(std::move(data_)); }

  T value_or(T alternative) const {
    return this->ok() ? this->value() : std::move(alternative);
  }

 private:
  absl::variant<absl::Status, T> data_;
};

}  // phst_google_elisp

#endif
