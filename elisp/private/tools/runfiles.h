// Copyright 2020-2025 Google LLC
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

#ifndef ELISP_PRIVATE_TOOLS_RUNFILES_H_
#define ELISP_PRIVATE_TOOLS_RUNFILES_H_

#if !defined __cplusplus || __cplusplus < 201703L
#  error this file requires at least C++17
#endif

#include <memory>
#include <string_view>
#include <utility>

#include "absl/base/nullability.h"
#include "absl/log/die_if_null.h"
#include "absl/status/statusor.h"
#include "absl/types/span.h"
#include "cc/runfiles/runfiles.h"

#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/system.h"

namespace rules_elisp {

enum class ExecutableKind { kBinary, kTest };

class Runfiles final {
 public:
  static absl::StatusOr<Runfiles> Create(
      ExecutableKind kind, std::string_view source_repository,
      absl::Span<const NativeStringView> original_argv);

  Runfiles(const Runfiles&) = delete;
  Runfiles& operator=(const Runfiles&) = delete;

  Runfiles(Runfiles&&) = default;
  Runfiles& operator=(Runfiles&&) = default;

  absl::StatusOr<NativeString> Resolve(std::string_view name) const;
  absl::StatusOr<Environment> Environ() const;

 private:
  using Impl = rules_cc::cc::runfiles::Runfiles;

  explicit Runfiles(absl_nonnull std::unique_ptr<Impl> impl)
      : impl_(std::move(ABSL_DIE_IF_NULL(impl))) {}

  absl_nullable std::unique_ptr<Impl> impl_;
};

}  // namespace rules_elisp

#endif  // ELISP_PROCESS_H_
