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

#ifndef ELISP_PRIVATE_TOOLS_MANIFEST_H_
#define ELISP_PRIVATE_TOOLS_MANIFEST_H_

#include <memory>
#include <vector>

#include "absl/base/nullability.h"
#include "absl/status/statusor.h"
#include "absl/types/span.h"

#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/system.h"

namespace rules_elisp {

class ManifestFile final {
 public:
  static absl::StatusOr<ManifestFile> Create(
      const Options& opts, absl::Span<const FileName> input_files,
      absl::Span<const FileName> output_files);

  ManifestFile(const ManifestFile&) = delete;
  ManifestFile& operator=(const ManifestFile&) = delete;
  ManifestFile(ManifestFile&&);
  ManifestFile& operator=(ManifestFile&&);

  ~ManifestFile() noexcept;

  void AppendArgs(std::vector<NativeString>& args) const;

 private:
  class Impl;

  explicit ManifestFile() = default;
  explicit ManifestFile(absl_nonnull std::unique_ptr<const Impl> impl);

  absl_nullable std::unique_ptr<const Impl> impl_;
};

}  // namespace rules_elisp

#endif  // ELISP_PRIVATE_TOOLS_MANIFEST_H_
