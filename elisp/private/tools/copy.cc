// Copyright 2020-2026 Google LLC
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

#include "elisp/private/tools/copy.h"

#include <fstream>
#include <ios>
#include <locale>
#include <string>

#include "absl/status/status.h"
#include "absl/status/status_macros.h"
#include "absl/strings/str_format.h"

#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/system.h"

namespace rules_elisp {

absl::Status CopyFiles(const FileName& from, const FileName& to,
                       const FileName& list) {
  ABSL_ASSIGN_OR_RETURN(const FileName from_abs, from.MakeAbsolute());
  ABSL_ASSIGN_OR_RETURN(const FileName to_abs, to.MakeAbsolute());

  std::ifstream stream(list.string(), std::ios::in | std::ios::binary);
  if (!stream.is_open() || !stream.good()) {
    return absl::FailedPreconditionError(
        absl::StrFormat("Cannot open parameter file %s for reading", list));
  }
  stream.imbue(std::locale::classic());

  std::string line;
  while (std::getline(stream, line)) {
    ABSL_ASSIGN_OR_RETURN(const NativeString native,
                          ToNative(line, Encoding::kUtf8));
    ABSL_ASSIGN_OR_RETURN(FileName from_file, FileName::FromString(native));
    ABSL_ASSIGN_OR_RETURN(from_file, from_file.MakeAbsolute());
    ABSL_ASSIGN_OR_RETURN(const FileName relative,
                          from_file.MakeRelative(from_abs));
    ABSL_ASSIGN_OR_RETURN(const FileName to_file, to_abs.Join(relative));
    ABSL_ASSIGN_OR_RETURN(const FileName parent, to_file.Parent());
    ABSL_RETURN_IF_ERROR(CreateDirectories(parent));
    ABSL_RETURN_IF_ERROR(CopyFile(from_file, to_file));
  }

  if (stream.bad() || !stream.eof()) {
    return absl::FailedPreconditionError(
        absl::StrFormat("Cannot read parameter file %s", list));
  }

  return absl::OkStatus();
}

}  // namespace rules_elisp
