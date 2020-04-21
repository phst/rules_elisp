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

TEST(Executor, RunBinaryWrap) {
  const char *const argv[2] = {"unused", nullptr};
  const char *const envp[1] = {nullptr};
  auto status_or_executor = Executor::CreateForTest(1, argv, envp);
  ASSERT_TRUE(status_or_executor.ok()) << status_or_executor.status();
  auto& executor = status_or_executor.value();
  const auto status_or_code = executor.RunBinary(
      "phst_rules_elisp/emacs/wrap/wrap", Mode::kWrap, {"phst_rules_elisp"}, {},
      {"phst_rules_elisp/emacs/exec.h"});
  ASSERT_TRUE(status_or_code.ok()) << status_or_code.status();
  EXPECT_THAT(status_or_code.value(), Eq(0));
}

}  // namespace
}  // namespace phst_rules_elisp
