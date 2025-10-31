// Copyright 2021-2023, 2025 Google LLC
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

#include "elisp/private/tools/manifest.h"

#include <algorithm>
#include <cstddef>
#include <string>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/status/status_matchers.h"
#include "absl/status/statusor.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/system.h"

namespace rules_elisp {
namespace {

using absl_testing::IsOk;
using absl_testing::IsOkAndHolds;
using ::testing::ElementsAre;
using ::testing::StartsWith;

// Create a string from a literal that can contain embedded null characters.
template <std::size_t N>
NativeString String(const NativeChar (&string)[N]) {
  static_assert(N > 0);
  return NativeString(string, N - 1);
}

TEST(ManifestFileTest, Create) {
  Options opts;
  opts.wrapper = RULES_ELISP_NATIVE_LITERAL("wrapper");
  opts.mode = ToolchainMode::kWrap;
  opts.tags = {
      RULES_ELISP_NATIVE_LITERAL("tag-1"),
      String(RULES_ELISP_NATIVE_LITERAL("tag-2 \t\n\r\f äα𝐴🐈'\0\\\"")),
  };
  opts.load_path = {RULES_ELISP_NATIVE_LITERAL("load-dir")};
  opts.load_files = {RULES_ELISP_NATIVE_LITERAL("load-file")};
  opts.data_files = {RULES_ELISP_NATIVE_LITERAL("data-file")};
  const FileName in_1 =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("in-1")).value();
  const FileName in_2 =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("in-2")).value();
  const FileName out =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("out äα𝐴🐈'")).value();
  const absl::StatusOr<ManifestFile> file =
      ManifestFile::Create(opts, {in_1, in_2}, {out});
  ASSERT_THAT(file, IsOk());

  std::vector<NativeString> args;
  file->AppendArgs(args);
  const NativeString prefix = RULES_ELISP_NATIVE_LITERAL("--manifest=");
  ASSERT_THAT(
      args, ElementsAre(StartsWith(prefix), RULES_ELISP_NATIVE_LITERAL("--")));

  std::string want = R"js({
                            "root": "RUNFILES_ROOT",
                            "loadPath": [
                              "load-dir"
                            ],
                            "inputFiles": [
                              "in-1",
                              "in-2",
                              "load-file",
                              "data-file"
                            ],
                            "outputFiles": [
                              "out~äα𝐴🐈'"
                            ],
                            "tags": [
                              "tag-1",
                              "tag-2~\t\n\r\f~äα𝐴🐈'\u0000\\\""
                            ]
                          })js";
  // Munge JSON literal so that it looks nice in the code.  Remove all
  // whitespace, then replace ~ with space characters.
  want.erase(std::remove(want.begin(), want.end(), ' '), want.end());
  want.erase(std::remove(want.begin(), want.end(), '\n'), want.end());
  absl::c_replace(want, '~', ' ');

  // Read entire manifest file into a buffer.
  NativeStringView name = args.at(0);
  name.remove_prefix(prefix.length());
  const absl::StatusOr<FileName> filename = FileName::FromString(name);
  ASSERT_THAT(filename, IsOk());
  EXPECT_THAT(ReadFile(*filename), IsOkAndHolds(want));
}

}  // namespace
}  // namespace rules_elisp
