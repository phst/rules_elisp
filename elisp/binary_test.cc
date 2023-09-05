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

#include <string>
#include <utility>
#include <vector>

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#endif
#ifdef _MSC_VER
#pragma warning(push, 3)
#endif
#include "absl/base/thread_annotations.h"
#include "absl/log/check.h"
#include "absl/synchronization/mutex.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "gmock/gmock.h"  // IWYU pragma: keep
#include "gtest/gtest.h"  // IWYU pragma: keep
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
#ifdef _MSC_VER
#pragma warning(pop)
#endif

#include "elisp/platform.h"
#include "elisp/process.h"

#ifndef BAZEL_CURRENT_REPOSITORY
#define BAZEL_CURRENT_REPOSITORY ""
#endif

// IWYU pragma: no_include "gtest/gtest_pred_impl.h"
// IWYU pragma: no_include <gmock/gmock-matchers.h>
// IWYU pragma: no_include <gtest/gtest-matchers.h>
// IWYU pragma: no_include <gtest/gtest-message.h>
// IWYU pragma: no_include <gtest/gtest-test-part.h>

namespace phst_rules_elisp {
namespace {

using ::testing::Eq;

class Wrapper final {
 public:
  static NativeString Get() ABSL_LOCKS_EXCLUDED(mu_) {
    return Singleton().DoGet();
  }

  static void Set(NativeString value) ABSL_LOCKS_EXCLUDED(mu_) {
    Singleton().DoSet(std::move(value));
  }

  Wrapper(const Wrapper&) = delete;
  Wrapper& operator=(const Wrapper&) = delete;

 private:
  static Wrapper& Singleton() {
    static Wrapper& wrapper = *new Wrapper;
    return wrapper;
  }

  Wrapper() = default;

  NativeString DoGet() ABSL_LOCKS_EXCLUDED(mu_) {
    absl::MutexLock lock(&mu_);
    return value_;
  }

  void DoSet(NativeString value) ABSL_LOCKS_EXCLUDED(mu_) {
    absl::MutexLock lock(&mu_);
    value_ = std::move(value);
  }

  NativeString value_ ABSL_GUARDED_BY(mu_);
  absl::Mutex mu_;
};


TEST(Executor, RunBinaryWrap) {
  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::CreateForTest(BAZEL_CURRENT_REPOSITORY);
  ASSERT_TRUE(runfiles.ok()) << runfiles.status();
  const NativeString wrapper = Wrapper::Get();
  ASSERT_FALSE(wrapper.empty()) << "missing wrapper command-line argument";
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

int PHST_RULES_ELISP_MAIN(int argc, phst_rules_elisp::NativeChar** argv) {
  testing::InitGoogleTest(&argc, argv);
  // Don’t use absl::ParseCommandLine since that doesn’t support wide strings.
  QCHECK_EQ(argc, 2) << "usage: binary_test WRAPPER";
  phst_rules_elisp::Wrapper::Set(argv[1]);
  return RUN_ALL_TESTS();
}
