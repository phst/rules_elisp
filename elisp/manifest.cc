// Copyright 2020, 2021 Google LLC
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

#include "elisp/manifest.h"

#include <cstdlib>
#include <iostream>
#include <string>
#include <utility>
#include <vector>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#pragma GCC diagnostic ignored "-Woverflow"
#include "absl/base/casts.h"
#include "absl/container/flat_hash_map.h"
#include "absl/random/random.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/types/optional.h"
#include "nlohmann/json.hpp"
#pragma GCC diagnostic pop

#include "elisp/algorithm.h"
#include "elisp/file.h"
#include "elisp/options.h"
#include "elisp/status.h"

namespace phst_rules_elisp {

absl::StatusOr<absl::optional<TempFile>> AddManifest(
    const Mode mode, std::vector<std::string>& args, absl::BitGen& random) {
  using Type = absl::optional<TempFile>;
  if (mode == Mode::kDirect) return absl::implicit_cast<Type>(absl::nullopt);
  ASSIGN_OR_RETURN(auto stream,
                   TempFile::Create(TempDir(), "manifest-*.json", random));
  args.push_back(absl::StrCat("--manifest=", stream.path()));
  args.push_back("--");
  return absl::implicit_cast<Type>(std::move(stream));
}

static void CheckRelative(const std::vector<std::string>& files) {
  for (const std::string& file : files) {
    if (IsAbsolute(file)) {
      std::clog << "filename " << file << " is absolute" << std::endl;
      std::abort();
    }
  }
}

absl::Status WriteManifest(const CommonOptions& opts,
                           std::vector<std::string> input_files,
                           const std::vector<std::string>& output_files,
                           TempFile& file) {
  CheckRelative(opts.load_path);
  CheckRelative(opts.load_files);
  const auto data_files = Sort(opts.data_files);
  CheckRelative(data_files);
  input_files.insert(input_files.end(),
                     opts.load_files.begin(), opts.load_files.end());
  input_files.insert(input_files.end(), data_files.begin(), data_files.end());
  const nlohmann::json json = {
      {"root", "RUNFILES_ROOT"},
      {"loadPath", opts.load_path},
      {"inputFiles", input_files},
      {"outputFiles", output_files},
      {"tags", Sort(opts.rule_tags)},
  };
  return file.Write(json.dump());
}

}  // namespace phst_rules_elisp
