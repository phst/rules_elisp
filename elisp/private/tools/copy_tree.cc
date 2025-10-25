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

#include "absl/algorithm/container.h"
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

static absl::Status Main(const NativeStringView readme,
                         const NativeStringView install) {
  absl::StatusOr<NativeString> readme_abs = rules_elisp::MakeAbsolute(readme);
  if (!readme_abs.ok()) return readme_abs.status();

  const absl::StatusOr<NativeString> install_abs =
      rules_elisp::MakeAbsolute(install);
  if (!install_abs.ok()) return install_abs.status();

  if constexpr (kWindows) {
    absl::c_replace(*readme_abs, RULES_ELISP_NATIVE_LITERAL('/'), kSeparator);
  }

  NativeStringView source = *readme_abs;
  const NativeStringView::size_type i = source.rfind(kSeparator);
  if (i == source.npos) {
    return absl::InvalidArgumentError(
        absl::StrFormat("Invalid README filename %s", *readme_abs));
  }
  source.remove_suffix(source.size() - i);

  // Bazel sometimes creates the output directory; remove it so that
  // CopyTree works correctly.
  const absl::Status status = RemoveDirectory(*install_abs);
  if (!status.ok() && !absl::IsNotFound(status)) return status;

  return CopyTree(source, *install_abs);
}

}  // namespace rules_elisp

int RULES_ELISP_MAIN(int argc, rules_elisp::NativeChar** argv) {
  absl::InitializeLog();
  absl::SetStderrThreshold(absl::LogSeverityAtLeast::kWarning);
  QCHECK_EQ(argc, 3);
  const absl::Status status = rules_elisp::Main(argv[1], argv[2]);
  if (!status.ok()) {
    LOG(ERROR) << status;
    return EXIT_FAILURE;
  }
}
