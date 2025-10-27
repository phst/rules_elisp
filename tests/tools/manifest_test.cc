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
#include <fstream>
#include <ios>
#include <locale>
#include <sstream>
#include <string>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/status/status_matchers.h"
#include "absl/status/statusor.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

#include "elisp/private/tools/platform.h"

namespace rules_elisp {

using absl_testing::IsOk;
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
  const NativeString in_1 = RULES_ELISP_NATIVE_LITERAL("in-1");
  const NativeString in_2 = RULES_ELISP_NATIVE_LITERAL("in-2");
  const NativeString out = RULES_ELISP_NATIVE_LITERAL("out äα𝐴🐈'");
  const absl::StatusOr<ManifestFile> file =
      ManifestFile::Create(opts, {in_1, in_2}, {out});
  ASSERT_THAT(file, IsOk());

  std::vector<NativeString> args;
  file->AppendArgs(args);
  const NativeString prefix = RULES_ELISP_NATIVE_LITERAL("--manifest=");
  ASSERT_THAT(
      args, ElementsAre(StartsWith(prefix), RULES_ELISP_NATIVE_LITERAL("--")));

  // Read entire manifest file into a buffer.
  NativeStringView name = args.at(0);
  name.remove_prefix(prefix.length());
  std::ifstream stream(NativeString(name), std::ios::in | std::ios::binary);
  stream.imbue(std::locale::classic());
  EXPECT_TRUE(stream.is_open());
  EXPECT_TRUE(stream.good());
  std::ostringstream buffer;
  buffer.imbue(std::locale::classic());
  EXPECT_TRUE(buffer.good());
  buffer << stream.rdbuf();
  EXPECT_TRUE(stream.good());
  EXPECT_TRUE(buffer.good());

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

  EXPECT_EQ(buffer.str(), want);
}

}  // namespace rules_elisp
