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

namespace phst_rules_elisp {
namespace {

using ::testing::Eq;

TEST(Executor, RunBinaryWrap) {
  const std::string argv0 = "unused";
  const std::vector<std::string> args = {
      "--wrapper=phst_rules_elisp/tests/wrap/wrap",
      "--mode=wrap",
      "--rule-tag=local",
      "--rule-tag=mytag",
      "--load-directory=phst_rules_elisp",
      "--data-file=phst_rules_elisp/elisp/binary.h",
      "--input-arg=2",
      "--output-arg=-1",
      "--",
      argv0,
      "--option",
      "elisp/binary.cc",
      "/:/tmp/output.dat",
  };
  EXPECT_THAT(RunBinary(argv0, args), Eq(0));
}

}  // namespace
}  // namespace phst_rules_elisp
