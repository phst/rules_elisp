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

#include <cstdlib>
#include <fstream>
#include <ios>
#include <locale>
#include <optional>
#include <string>
#include <utility>

#include "absl/base/log_severity.h"
#include "absl/log/check.h"
#include "absl/log/globals.h"
#include "absl/log/initialize.h"
#include "absl/log/log.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_format.h"

#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/system.h"

namespace rules_elisp {

static absl::Status Main(const NativeStringView readme_file,
                         const NativeStringView install_dir,
                         const NativeStringView srcs_file) {
  const absl::StatusOr<FileName> readme = FileName::FromString(readme_file);
  if (!readme.ok()) return readme.status();

  const absl::StatusOr<FileName> base = readme->Parent();
  if (!base.ok()) return base.status();

  absl::StatusOr<FileName> install = FileName::FromString(install_dir);
  if (!install.ok()) return install.status();

  std::optional<DosDevice> dos_device;
  if constexpr (kWindows) {
    absl::StatusOr<DosDevice> dev = DosDevice::Create(*install);
    if (!dev.ok()) return dev.status();
    absl::StatusOr<FileName> root =
        FileName::FromString(dev->name() + RULES_ELISP_NATIVE_LITERAL("\\"));
    if (!root.ok()) return root.status();
    install = *std::move(root);
    dos_device = *std::move(dev);
  }

  const absl::StatusOr<FileName> srcs = FileName::FromString(srcs_file);
  if (!srcs.ok()) return srcs.status();

  std::ifstream stream(srcs->string(), std::ios::in | std::ios::binary);
  if (!stream.is_open() || !stream.good()) {
    return absl::FailedPreconditionError(
        absl::StrFormat("Cannot open parameter file %v for reading", *srcs));
  }
  stream.imbue(std::locale::classic());

  std::string line;
  while (std::getline(stream, line)) {
    const absl::StatusOr<NativeString> native = ToNative(line, Encoding::kUtf8);
    if (!native.ok()) return native.status();
    const absl::StatusOr<FileName> from = FileName::FromString(*native);
    if (!from.ok()) return from.status();
    const absl::StatusOr<FileName> relative = from->MakeRelative(*base);
    if (!relative.ok()) return relative.status();
    const absl::StatusOr<FileName> to = install->Join(*relative);
    if (!to.ok()) return to.status();
    const absl::StatusOr<FileName> parent = to->Parent();
    if (!parent.ok()) return parent.status();
    if (const absl::Status status = CreateDirectories(*parent); !status.ok()) {
      return status;
    }
    if (const absl::Status status = CopyFile(*from, *to); !status.ok()) {
      return status;
    }
  }

  if (stream.bad() || !stream.eof()) {
    return absl::FailedPreconditionError(
        absl::StrFormat("Cannot read parameter file %v", *srcs));
  }

  return absl::OkStatus();
}

}  // namespace rules_elisp

int RULES_ELISP_MAIN(int argc, rules_elisp::NativeChar** argv) {
  absl::InitializeLog();
  absl::SetStderrThreshold(absl::LogSeverityAtLeast::kWarning);
  QCHECK_EQ(argc, 4);
  const absl::Status status = rules_elisp::Main(argv[1], argv[2], argv[3]);
  if (!status.ok()) {
    LOG(ERROR) << status;
    return EXIT_FAILURE;
  }
}
