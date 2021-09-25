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

#include "gmock/gmock.h"
#include "gtest/gtest.h"

#include "elisp/options.h"

namespace phst_rules_elisp {
namespace {

using ::testing::Eq;

TEST(Executor, RunBinaryWrap) {
  BinaryOptions opts;
  opts.wrapper = "phst_rules_elisp/tests/wrap/wrap";
  opts.mode = Mode::kWrap;
  opts.rule_tags = {"local", "mytag"};
  opts.load_path = {"phst_rules_elisp"};
  opts.data_files = {"phst_rules_elisp/elisp/binary.h"};
  opts.input_args = {2};
  opts.output_args = {-1};
  opts.argv = {"unused", "--option", "elisp/binary.cc", "/:/tmp/output.dat"};
  EXPECT_THAT(RunBinary(opts), Eq(0));
}

}  // namespace
}  // namespace phst_rules_elisp
