// Copyright 2020, 2021, 2022, 2023, 2024 Google LLC
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

#ifndef RULES_ELISP_ELISP_PROCESS_H
#define RULES_ELISP_ELISP_PROCESS_H

#if !defined __cplusplus || __cplusplus < 201703L
#  error this file requires at least C++17
#endif

#include <memory>
#include <string>
#include <string_view>  // IWYU pragma: keep

#ifdef __GNUC__
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wpedantic"
#  pragma GCC diagnostic ignored "-Wconversion"
#  pragma GCC diagnostic ignored "-Wsign-conversion"
#  pragma GCC diagnostic ignored "-Woverflow"
#endif
#ifdef _MSC_VER
#  pragma warning(push, 3)
#endif
#include "absl/base/nullability.h"
#include "absl/container/flat_hash_map.h"
#include "absl/status/statusor.h"
#include "absl/types/span.h"
#include "tools/cpp/runfiles/runfiles.h"
#ifdef __GNUC__
#  pragma GCC diagnostic pop
#endif
#ifdef _MSC_VER
#  pragma warning(pop)
#endif

#include "elisp/platform.h"

// IWYU pragma: no_include <__fwd/string_view.h>

namespace rules_elisp {

#ifdef _WIN32
struct CaseInsensitiveHash;
struct CaseInsensitiveEqual;
using Environment =
    absl::flat_hash_map<std::wstring, std::wstring, CaseInsensitiveHash,
                        CaseInsensitiveEqual>;
#else
using Environment = absl::flat_hash_map<std::string, std::string>;
#endif

enum class ExecutableKind { kBinary, kTest };

class Runfiles final {
 public:
  static absl::StatusOr<Runfiles> Create(ExecutableKind kind,
                                         std::string_view source_repository,
                                         NativeStringView argv0);
  absl::StatusOr<NativeString> Resolve(std::string_view name) const;
  absl::StatusOr<rules_elisp::Environment> Environment() const;

 private:
  using Impl = bazel::tools::cpp::runfiles::Runfiles;
  explicit Runfiles(absl::Nonnull<std::unique_ptr<Impl>> impl);
  absl::Nonnull<std::unique_ptr<Impl>> impl_;
};

absl::StatusOr<int> Run(std::string_view binary,
                        absl::Span<const NativeString> args,
                        const Runfiles& runfiles);

}  // namespace rules_elisp

#endif
