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

#include "elisp/private/tools/copy.h"

#include <fstream>
#include <ios>
#include <locale>
#include <optional>
#include <string>
#include <utility>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_format.h"

#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/system.h"

namespace rules_elisp {

absl::Status CopyFiles(const FileName& from, const FileName& to,
                       const FileName& list) {
  FileName dest = to;

  std::optional<DosDevice> dos_device;
  if constexpr (kWindows) {
    absl::StatusOr<DosDevice> dev = DosDevice::Create(to);
    if (!dev.ok()) return dev.status();
    absl::StatusOr<FileName> root =
        FileName::FromString(dev->name() + RULES_ELISP_NATIVE_LITERAL("\\"));
    if (!root.ok()) return root.status();
    dest = *std::move(root);
    dos_device = *std::move(dev);
  }

  std::ifstream stream(list.string(), std::ios::in | std::ios::binary);
  if (!stream.is_open() || !stream.good()) {
    return absl::FailedPreconditionError(
        absl::StrFormat("Cannot open parameter file %v for reading", list));
  }
  stream.imbue(std::locale::classic());

  std::string line;
  while (std::getline(stream, line)) {
    const absl::StatusOr<NativeString> native = ToNative(line, Encoding::kUtf8);
    if (!native.ok()) return native.status();
    const absl::StatusOr<FileName> from_file = FileName::FromString(*native);
    if (!from_file.ok()) return from_file.status();
    const absl::StatusOr<FileName> relative = from_file->MakeRelative(from);
    if (!relative.ok()) return relative.status();
    const absl::StatusOr<FileName> to_file = dest.Join(*relative);
    if (!to_file.ok()) return to_file.status();
    const absl::StatusOr<FileName> parent = to_file->Parent();
    if (!parent.ok()) return parent.status();
    if (const absl::Status status = CreateDirectories(*parent); !status.ok()) {
      return status;
    }
    if (const absl::Status status = CopyFile(*from_file, *to_file);
        !status.ok()) {
      return status;
    }
  }

  if (stream.bad() || !stream.eof()) {
    return absl::FailedPreconditionError(
        absl::StrFormat("Cannot read parameter file %v", list));
  }

  return absl::OkStatus();
}

}  // namespace rules_elisp
