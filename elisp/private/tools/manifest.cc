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
#include "elisp/private/tools/system.h"

namespace rules_elisp {

static absl::StatusOr<std::vector<FileName>> ConvertNames(
    const absl::Span<const NativeString> range) {
  std::vector<FileName> result;
  for (const NativeString& file : range) {
    absl::StatusOr<FileName> name = FileName::FromString(file);
    if (!name.ok()) return name.status();
    if (name->IsAbsolute()) {
      return absl::InvalidArgumentError(
          absl::StrFormat("Filename %v is absolute", name));
    }
    result.push_back(*std::move(name));
  }
  return result;
}

static absl::Status Assign(
    google::protobuf::RepeatedPtrField<std::string>& field,
    const absl::Span<const NativeStringView> range) {
  for (const NativeStringView& elt : range) {
    absl::StatusOr<std::string> utf8 = ToNarrow(elt, Encoding::kUtf8);
    if (!utf8.ok()) return utf8.status();
    field.Add(*std::move(utf8));
  }
  return absl::OkStatus();
}

static absl::Status Assign(
    google::protobuf::RepeatedPtrField<std::string>& field,
    const absl::Span<const FileName> range) {
  for (const FileName& elt : range) {
    NativeString string = elt.string();
    if (kWindows && !elt.IsAbsolute()) {
      // Runfile names are slash-separated on all systems.
      absl::c_replace(string, kSeparator, RULES_ELISP_NATIVE_LITERAL('/'));
    }
    absl::StatusOr<std::string> utf8 = ToNarrow(string, Encoding::kUtf8);
    if (!utf8.ok()) return utf8.status();
    field.Add(*std::move(utf8));
  }
  return absl::OkStatus();
}

static absl::Status Write(const Options& opts,
                          const absl::Span<const FileName> input_files,
                          const absl::Span<const FileName> output_files,
                          TemporaryFile& file) {
  const absl::StatusOr<std::vector<FileName>> load_path =
      ConvertNames(opts.load_path);
  if (!load_path.ok()) return load_path.status();

  const absl::StatusOr<std::vector<FileName>> load_files =
      ConvertNames(opts.load_files);
  if (!load_files.ok()) return load_files.status();

  absl::StatusOr<std::vector<FileName>> data_files =
      ConvertNames(opts.data_files);
  if (!data_files.ok()) return data_files.status();
  absl::c_sort(*data_files);

  std::vector<FileName> all_input_files(input_files.cbegin(),
                                        input_files.cend());
  all_input_files.insert(all_input_files.end(), load_files->cbegin(),
                         load_files->cend());
  all_input_files.insert(all_input_files.end(), data_files->cbegin(),
                         data_files->cend());

  std::vector<NativeStringView> tags(opts.tags.cbegin(), opts.tags.cend());
  absl::c_sort(tags);

  Manifest manifest;
  manifest.set_root(Manifest::RUNFILES_ROOT);
  if (const absl::Status status =
          Assign(*manifest.mutable_load_path(), *load_path);
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
    const Options& opts, const absl::Span<const FileName> input_files,
    const absl::Span<const FileName> output_files) {
  if (opts.mode == ToolchainMode::kDirect) return ManifestFile();
  absl::StatusOr<TemporaryFile> file = TemporaryFile::Create();
  if (!file.ok()) return file.status();
  const absl::Status status = Write(opts, input_files, output_files, *file);
  if (!status.ok()) return status;
  return ManifestFile(*std::move(file));
}

void ManifestFile::AppendArgs(std::vector<NativeString>& args) const {
  if (!file_.has_value()) return;
  args.push_back(RULES_ELISP_NATIVE_LITERAL("--manifest=") +
                 file_->name().string());
  args.push_back(RULES_ELISP_NATIVE_LITERAL("--"));
}

}  // namespace rules_elisp
