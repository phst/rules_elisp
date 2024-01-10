// Copyright 2020, 2021, 2022, 2023 Google LLC
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

#include "elisp/binary.h"

#include <cstdlib>
#include <vector>

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#ifdef _MSC_VER
#pragma warning(push, 3)
#endif
#include "absl/log/log.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
#ifdef _MSC_VER
#pragma warning(pop)
#endif

#include "elisp/process.h"

namespace phst_rules_elisp {

static absl::StatusOr<int> RunBinaryImpl(
    const NativeString& argv0, const std::vector<NativeString>& args) {
  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(BAZEL_CURRENT_REPOSITORY, argv0);
  if (!runfiles.ok()) return runfiles.status();
  return Run("phst_rules_elisp/elisp/run_binary", args, *runfiles);
}

int RunBinary(const NativeString& argv0,
              const std::vector<NativeString>& args) {
  const absl::StatusOr<int> status_or_code = RunBinaryImpl(argv0, args);
  if (!status_or_code.ok()) {
    LOG(ERROR) << status_or_code.status();
    return EXIT_FAILURE;
  }
  return status_or_code.value();
}

}  // namespace phst_rules_elisp
