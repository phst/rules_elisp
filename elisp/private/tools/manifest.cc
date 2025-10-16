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

#include "elisp/private/tools/manifest.h"

#include <initializer_list>
#include <string>
#include <utility>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_format.h"
#include "absl/types/span.h"
#include "google/protobuf/json/json.h"
#include "google/protobuf/repeated_ptr_field.h"

#include "elisp/private/tools/manifest.pb.h"
#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/strings.h"
#include "elisp/private/tools/system.h"

namespace rules_elisp {

template <typename Range>
absl::Status CheckRelative(const Range& range) {
  for (const auto& file : range) {
    if (file.empty()) return absl::InvalidArgumentError("Empty filename");
    if (ContainsNull(file)) {
      return absl::InvalidArgumentError(
          absl::StrFormat("Filename %s contains null character", file));
    }
    if (IsAbsolute(file)) {
      return absl::InvalidArgumentError(
          absl::StrFormat("Filename %s is absolute", file));
    }
  }
  return absl::OkStatus();
}

template <typename Range>
absl::Status Assign(google::protobuf::RepeatedPtrField<std::string>& field,
                    const Range& range) {
  for (const auto& elt : range) {
    absl::StatusOr<std::string> utf8 = ToNarrow(elt, Encoding::kUtf8);
    if (!utf8.ok()) return utf8.status();
    field.Add(std::move(*utf8));
  }
  return absl::OkStatus();
}

static absl::Status Write(const CommonOptions& opts,
                          const absl::Span<const NativeString> input_files,
                          const absl::Span<const NativeString> output_files,
                          TemporaryFile& file) {
  if (const absl::Status status = CheckRelative(opts.load_path); !status.ok()) {
    return status;
  }
  if (const absl::Status status = CheckRelative(opts.load_files);
      !status.ok()) {
    return status;
  }

  std::vector<NativeStringView> data_files(opts.data_files.begin(),
                                           opts.data_files.end());
  absl::c_sort(data_files);
  if (const absl::Status status = CheckRelative(data_files); !status.ok()) {
    return status;
  }

  std::vector<NativeStringView> all_input_files(input_files.cbegin(),
                                                input_files.cend());
  all_input_files.insert(all_input_files.end(), opts.load_files.begin(),
                         opts.load_files.end());
  all_input_files.insert(all_input_files.end(), data_files.begin(),
                         data_files.end());

  std::vector<NativeStringView> tags(opts.tags.begin(), opts.tags.end());
  absl::c_sort(tags);

  Manifest manifest;
  manifest.set_root(Manifest::RUNFILES_ROOT);
  if (const absl::Status status =
          Assign(*manifest.mutable_load_path(), opts.load_path);
      !status.ok()) {
    return status;
  }
  if (const absl::Status status =
          Assign(*manifest.mutable_input_files(), all_input_files);
      !status.ok()) {
    return status;
  }
  if (const absl::Status status =
          Assign(*manifest.mutable_output_files(), output_files);
      !status.ok()) {
    return status;
  }
  if (const absl::Status status = Assign(*manifest.mutable_tags(), tags);
      !status.ok()) {
    return status;
  }

  std::string json;
  const absl::Status status =
      google::protobuf::json::MessageToJsonString(manifest, &json);
  if (!status.ok()) return status;

  return file.Write(json);
}

absl::StatusOr<ManifestFile> ManifestFile::Create(
    const CommonOptions& opts, const absl::Span<const NativeString> input_files,
    const absl::Span<const NativeString> output_files) {
  if (opts.mode == ToolchainMode::kDirect) return ManifestFile();
  absl::StatusOr<TemporaryFile> file = TemporaryFile::Create();
  if (!file.ok()) return file.status();
  const absl::Status status = Write(opts, input_files, output_files, *file);
  if (!status.ok()) return status;
  return ManifestFile(std::move(*file));
}

void ManifestFile::AppendArgs(std::vector<NativeString>& args) const {
  if (!file_.has_value()) return;
  args.push_back(RULES_ELISP_NATIVE_LITERAL("--manifest=") + file_->name());
  args.push_back(RULES_ELISP_NATIVE_LITERAL("--"));
}

}  // namespace rules_elisp
