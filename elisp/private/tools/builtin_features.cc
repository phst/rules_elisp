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

#include <algorithm>
#include <cstdlib>
#include <fstream>
#include <ios>
#include <locale>
#include <regex>
#include <string>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/base/log_severity.h"
#include "absl/log/check.h"
#include "absl/log/globals.h"
#include "absl/log/initialize.h"
#include "absl/log/log.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_format.h"
#include "google/protobuf/io/zero_copy_stream_impl.h"
#include "google/protobuf/json/json.h"

#include "elisp/private/tools/builtin_features.pb.h"
#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/system.h"

namespace rules_elisp {

static absl::Status Extract(const NativeStringView param_file) {
  std::ifstream param_stream(NativeString(param_file),
                             std::ios::in | std::ios::binary);
  if (!param_stream.is_open() || !param_stream.good()) {
    return absl::FailedPreconditionError(
        absl::StrFormat("Cannot open parameter file %s", param_file));
  }
  param_stream.imbue(std::locale::classic());

  std::string param_line;
  std::getline(param_stream, param_line);
  const absl::StatusOr<NativeString> output_file =
      ToNative(param_line, Encoding::kUtf8);
  if (!output_file.ok()) return output_file.status();

  std::regex regex(R"(\(provide '([-/\w]+)\))");
  std::vector<std::string> features_vector;
  while (std::getline(param_stream, param_line)) {
    const absl::StatusOr<NativeString> lisp_file =
        ToNative(param_line, Encoding::kUtf8);
    if (!lisp_file.ok()) return lisp_file.status();
    std::ifstream lisp_stream(*lisp_file, std::ios::in | std::ios::binary);
    if (!lisp_stream.is_open() || !lisp_stream.good()) {
      return absl::FailedPreconditionError(
          absl::StrFormat("Cannot open Lisp file %s", *lisp_file));
    }
    lisp_stream.imbue(std::locale::classic());
    std::string lisp_line;
    while (std::getline(lisp_stream, lisp_line)) {
      std::smatch match;
      if (std::regex_search(lisp_line, match, regex,
                            std::regex_constants::match_continuous)) {
        features_vector.push_back(match.str(1));
      }
    }
    if (lisp_stream.bad() || !lisp_stream.eof()) {
      return absl::FailedPreconditionError(
          absl::StrFormat("Cannot read Lisp file %s", *lisp_file));
    }
  }

  if (param_stream.bad() || !param_stream.eof()) {
    return absl::FailedPreconditionError(
        absl::StrFormat("Cannot read parameter file %s", param_file));
  }
  param_stream.close();

  if (features_vector.empty()) {
    return absl::NotFoundError("No builtin features found");
  }
  absl::c_sort(features_vector);
  BuiltinFeatures features_proto;
  features_proto.mutable_builtin_features()->Assign(
      features_vector.begin(),
      std::unique(features_vector.begin(), features_vector.end()));

  std::ofstream output_stream(
      *output_file, std::ios::out | std::ios::trunc | std::ios::binary);
  if (!output_stream.is_open() || !output_stream.good()) {
    return absl::FailedPreconditionError(
        absl::StrFormat("Cannot open output file %s", param_file));
  }
  output_stream.imbue(std::locale::classic());

  google::protobuf::io::OstreamOutputStream adapter(&output_stream);
  const absl::Status status =
      google::protobuf::json::MessageToJsonStream(features_proto, &adapter);
  if (!status.ok()) return status;

  output_stream.flush();
  if (!output_stream.good()) {
    return absl::FailedPreconditionError(
        absl::StrFormat("Cannot write output file %s", param_file));
  }
  return absl::OkStatus();
}

}  // namespace rules_elisp

int RULES_ELISP_MAIN(int argc, rules_elisp::NativeChar** argv) {
  absl::InitializeLog();
  absl::SetStderrThreshold(absl::LogSeverityAtLeast::kWarning);
  QCHECK_EQ(argc, 2);
  const absl::Status status = rules_elisp::Extract(argv[1]);
  if (!status.ok()) {
    LOG(ERROR) << status;
    return EXIT_FAILURE;
  }
}
