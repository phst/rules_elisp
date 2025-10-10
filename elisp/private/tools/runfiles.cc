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

#include "elisp/private/tools/runfiles.h"

#include <cstdlib>
#include <memory>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/base/nullability.h"
#include "absl/log/check.h"
#include "absl/log/log.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/types/span.h"
#include "cc/runfiles/runfiles.h"

#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/strings.h"
#include "elisp/private/tools/system.h"

namespace rules_elisp {

absl::StatusOr<Runfiles> Runfiles::Create(
    const ExecutableKind kind, const std::string_view source_repository,
    const absl::Span<const NativeStringView> original_argv) {
  if (const absl::Status status = CheckASCII(source_repository); !status.ok()) {
    return status;
  }
  const absl::StatusOr<std::string> argv0 =
      original_argv.empty() ? std::string() : ToNarrow(original_argv.front());
  if (!argv0.ok()) return argv0.status();
  std::string error;
  absl_nullable std::unique_ptr<Impl> impl;
  switch (kind) {
    case ExecutableKind::kBinary:
      impl.reset(Impl::Create(*argv0, std::string(source_repository), &error));
      break;
    case ExecutableKind::kTest:
      impl.reset(Impl::CreateForTest(std::string(source_repository), &error));
      break;
    default:
      LOG(FATAL) << "invalid runfiles mode "
                 << static_cast<std::underlying_type_t<ExecutableKind>>(kind);
      break;
  }
  if (impl == nullptr) {
    return absl::FailedPreconditionError(
        absl::StrCat("couldn’t create runfiles: ", error));
  }
  return Runfiles(std::move(impl));
}

absl::StatusOr<NativeString> Runfiles::Resolve(
    const std::string_view name) const {
  CHECK_NE(impl_, nullptr);
  if (const absl::Status status = CheckASCII(name); !status.ok()) return status;
  std::string resolved = impl_->Rlocation(std::string(name));
  if (resolved.empty()) {
    return absl::NotFoundError(absl::StrCat("runfile not found: ", name));
  }
  if constexpr (kWindows) absl::c_replace(resolved, '/', '\\');
  absl::StatusOr<NativeString> native = ToNative(resolved);
  if (!native.ok()) return native.status();
  return MakeAbsolute(*native);
}

absl::StatusOr<Environment> Runfiles::Environ() const {
  CHECK_NE(impl_, nullptr);
  std::vector<std::pair<NativeString, NativeString>> pairs;
  for (const auto& [narrow_key, narrow_value] : impl_->EnvVars()) {
    const absl::StatusOr<NativeString> key = ToNative(narrow_key);
    if (!key.ok()) return key.status();
    const absl::StatusOr<NativeString> value = ToNative(narrow_value);
    if (!value.ok()) return value.status();
    pairs.emplace_back(*key, *value);
  }
  return Environment::Create(pairs.cbegin(), pairs.cend());
}

}  // namespace rules_elisp
