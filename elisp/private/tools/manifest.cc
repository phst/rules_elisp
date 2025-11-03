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

#include <fstream>
#include <ios>
#include <locale>
#include <memory>
#include <ostream>
#include <string>
#include <utility>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/base/nullability.h"
#include "absl/log/check.h"
#include "absl/log/die_if_null.h"
#include "absl/log/log.h"
#include "absl/memory/memory.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_format.h"
#include "absl/types/span.h"
#include "google/protobuf/io/zero_copy_stream_impl.h"
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
          absl::StrFormat("Filename %s is absolute", *name));
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
                          std::ostream& file) {
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

  google::protobuf::io::OstreamOutputStream stream(&file);
  const absl::Status status =
      google::protobuf::json::MessageToJsonStream(manifest, &stream);
  if (!status.ok()) return status;
  file.flush();
  if (!file.good()) {
    return absl::DataLossError("Cannot write manifest file");
  }
  return absl::OkStatus();
}

class ManifestFile::Impl final {
 public:
  static absl::StatusOr<absl_nonnull std::unique_ptr<const Impl>> Create(
      const Options& opts, const absl::Span<const FileName> input_files,
      const absl::Span<const FileName> output_files) {
    CHECK_EQ(opts.mode, ToolchainMode::kWrap);
    const absl::StatusOr<FileName> directory = CreateTemporaryDirectory();
    if (!directory.ok()) return directory.status();
    absl::StatusOr<FileName> name =
        directory->Child(RULES_ELISP_NATIVE_LITERAL("manifest.json"));
    if (!name.ok()) return name.status();
    std::ofstream file(name->string(),
                       std::ios::out | std::ios::trunc | std::ios::binary);
    if (!file.is_open() || !file.good()) {
      return absl::FailedPreconditionError(
          absl::StrFormat("Cannot open manifest file %s for writing", *name));
    }
    file.imbue(std::locale::classic());
    const absl::Status status = Write(opts, input_files, output_files, file);
    if (!status.ok()) return status;
    return absl::WrapUnique(
        new const Impl(*std::move(directory), *std::move(name)));
  }

  Impl(const Impl&) = delete;
  Impl& operator=(const Impl&) = delete;
  Impl(Impl&&) = delete;
  Impl& operator=(Impl&&) = delete;

  ~Impl() noexcept {
    const absl::Status status = RemoveTree(directory_);
    if (!status.ok()) LOG(ERROR) << status;
  }

  void AppendArgs(std::vector<NativeString>& args) const {
    args.push_back(RULES_ELISP_NATIVE_LITERAL("--manifest=") + file_.string());
    args.push_back(RULES_ELISP_NATIVE_LITERAL("--"));
  }

 private:
  explicit Impl(FileName directory, FileName file)
      : directory_(std::move(directory)), file_(std::move(file)) {}

  FileName directory_;
  FileName file_;
};

absl::StatusOr<ManifestFile> ManifestFile::Create(
    const Options& opts, const absl::Span<const FileName> input_files,
    const absl::Span<const FileName> output_files) {
  if (opts.mode == ToolchainMode::kDirect) return ManifestFile();
  absl::StatusOr<absl_nonnull std::unique_ptr<const Impl>> impl =
      Impl::Create(opts, input_files, output_files);
  if (!impl.ok()) return impl.status();
  return ManifestFile(*std::move(impl));
}

ManifestFile::ManifestFile(absl_nonnull std::unique_ptr<const Impl> impl)
    : impl_(std::move(ABSL_DIE_IF_NULL(impl))) {}

ManifestFile::ManifestFile(ManifestFile&&) = default;
ManifestFile& ManifestFile::operator=(ManifestFile&&) = default;

ManifestFile::~ManifestFile() noexcept = default;

void ManifestFile::AppendArgs(std::vector<NativeString>& args) const {
  if (impl_ == nullptr) return;
  impl_->AppendArgs(args);
}

}  // namespace rules_elisp
