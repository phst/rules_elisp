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

#include "emacs/exec.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace phst_rules_elisp {
namespace {

using ::testing::Eq;

TEST(executor, run_binary_wrap) {
  const char *const argv[2] = {"unused", nullptr};
  const char *const envp[1] = {nullptr};
  EXPECT_THAT(executor(for_test(), 1, argv, envp)
                  .run_binary("phst_rules_elisp/emacs/wrap/wrap", mode::wrap,
                              {"phst_rules_elisp"}, {},
                              {"phst_rules_elisp/emacs/exec.h"}),
              Eq(0));
}

}  // namespace
}  // namespace phst_rules_elisp
