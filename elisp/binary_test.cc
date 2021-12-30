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

#include "elisp/binary.h"

#include <string>
#include <vector>

#include "gmock/gmock.h"
#include "gtest/gtest.h"

#include "elisp/process.h"

namespace phst_rules_elisp {
namespace {

using ::testing::Eq;

TEST(Executor, RunBinaryWrap) {
  const absl::StatusOr<Runfiles> runfiles = Runfiles::CreateForTest();
  ASSERT_TRUE(runfiles.ok()) << runfiles.status();
  NativeString wrapper = PHST_RULES_ELISP_NATIVE_LITERAL(
      "phst_rules_elisp/elisp/test_wrap_launcher");
#ifdef PHST_RULES_ELISP_WINDOWS
  wrapper += L".exe";
#endif
  const NativeString argv0 = PHST_RULES_ELISP_NATIVE_LITERAL("unused");
  const absl::StatusOr<NativeString> input_file =
      runfiles->Resolve("phst_rules_elisp/elisp/binary.cc");
  ASSERT_TRUE(input_file.ok()) << input_file.status();
  const std::vector<NativeString> args = {
      PHST_RULES_ELISP_NATIVE_LITERAL("--wrapper=") + wrapper,
      PHST_RULES_ELISP_NATIVE_LITERAL("--mode=wrap"),
      PHST_RULES_ELISP_NATIVE_LITERAL("--rule-tag=local"),
      PHST_RULES_ELISP_NATIVE_LITERAL("--rule-tag=mytag"),
      PHST_RULES_ELISP_NATIVE_LITERAL("--load-directory=phst_rules_elisp"),
      PHST_RULES_ELISP_NATIVE_LITERAL(
          "--data-file=phst_rules_elisp/elisp/binary.h"),
      PHST_RULES_ELISP_NATIVE_LITERAL("--input-arg=2"),
      PHST_RULES_ELISP_NATIVE_LITERAL("--output-arg=-1"),
      PHST_RULES_ELISP_NATIVE_LITERAL("--"),
      argv0,
      PHST_RULES_ELISP_NATIVE_LITERAL("--option"),
      *input_file,
      PHST_RULES_ELISP_NATIVE_LITERAL(" \t\n\r\f äα𝐴🐈'\\\""),
#ifdef PHST_RULES_ELISP_WINDOWS
      L"/:C:\\Temp\\output.dat",
#else
      "/:/tmp/output.dat",
#endif
  };
  EXPECT_THAT(RunBinary(argv0, args), Eq(0));
}

}  // namespace
}  // namespace phst_rules_elisp
