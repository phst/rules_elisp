// Copyright 2020 Google LLC
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

#include "elisp/exec.h"

#include "absl/base/macros.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace phst_rules_elisp {
namespace {

using ::testing::Eq;

TEST(Executor, RunBinaryWrap) {
  const char *const argv[] = {"unused", "--option", "elisp/exec.cc",
                              "/:/tmp/output.dat", nullptr};
  EXPECT_THAT(RunBinary("phst_rules_elisp/tests/wrap/wrap", Mode::kWrap,
                        {"local", "mytag"}, {"phst_rules_elisp"}, {},
                        {"phst_rules_elisp/elisp/exec.h"}, {2}, {-1},
                        ABSL_ARRAYSIZE(argv) - 1, argv),
              Eq(0));
}

}  // namespace
}  // namespace phst_rules_elisp
