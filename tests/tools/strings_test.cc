// Copyright 2025 Philipp Stephani
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

#include "elisp/private/tools/strings.h"

#include <cstddef>
#include <string_view>

#include "absl/base/attributes.h"
#include "absl/status/status.h"
#include "absl/status/status_matchers.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

#include "elisp/private/tools/platform.h"

namespace rules_elisp {

using absl_testing::IsOk;
using absl_testing::IsOkAndHolds;
using absl_testing::StatusIs;

// Helper function template to create string views from literals with embedded null characters.
template <typename Char, std::size_t Size>
std::basic_string_view<Char> View(
    const Char (&string ABSL_ATTRIBUTE_LIFETIME_BOUND)[Size]) {
  static_assert(Size > 0);
  return std::basic_string_view<Char>(string, Size - 1);
}

TEST(QuoteTest, QuotesStrings) {
  // We can’t use raw string literals here because of
  // https://developercommunity.visualstudio.com/t/c2017-illegal-escape-sequence-when-using-in-a-raw/919371.
  EXPECT_EQ(Quote("Foó \"\\"), "\"Foó \\\"\\\\\"");
  EXPECT_EQ(Quote(L"Foó \"\\"), "L\"Foó \\\"\\\\\"");
}

TEST(CheckAsciiTest, AcceptsAscii) {
  EXPECT_THAT(CheckAscii(""), IsOk());
  EXPECT_THAT(CheckAscii(L""), IsOk());
  EXPECT_THAT(CheckAscii("Foo"), IsOk());
  EXPECT_THAT(CheckAscii(L"Foo"), IsOk());
}

TEST(CheckAsciiTest, RejectsNonAscii) {
  EXPECT_THAT(CheckAscii("Foó"), StatusIs(absl::StatusCode::kInvalidArgument));
  EXPECT_THAT(CheckAscii(L"Foó"), StatusIs(absl::StatusCode::kInvalidArgument));
}

TEST(CheckNullTest, AcceptsStringsWithoutEmbeddedNull) {
  EXPECT_THAT(CheckNull(""), IsOk());
  EXPECT_THAT(CheckNull(L""), IsOk());
  EXPECT_THAT(CheckNull("foo"), IsOk());
  EXPECT_THAT(CheckNull(L"foo"), IsOk());
}

TEST(CheckNullTest, RejectsStringsWithEmbeddedNull) {
  EXPECT_THAT(CheckNull(View("\0")),
              StatusIs(absl::StatusCode::kInvalidArgument));
  EXPECT_THAT(CheckNull(View(L"\0")),
              StatusIs(absl::StatusCode::kInvalidArgument));
}

TEST(ToNarrowTest, AcceptsAscii) {
  EXPECT_THAT(ToNarrow(RULES_ELISP_NATIVE_LITERAL("")), IsOkAndHolds(""));
  EXPECT_THAT(ToNarrow(RULES_ELISP_NATIVE_LITERAL("Foo")), IsOkAndHolds("Foo"));
}

TEST(ToNarrowTest, RejectsNonAscii) {
  if constexpr (kWindows) {
    EXPECT_THAT(ToNarrow(RULES_ELISP_NATIVE_LITERAL("Foó")),
                StatusIs(absl::StatusCode::kInvalidArgument));
  }
}

TEST(ToNativeTest, AcceptsAscii) {
  EXPECT_THAT(ToNative(""), IsOkAndHolds(RULES_ELISP_NATIVE_LITERAL("")));
  EXPECT_THAT(ToNative("Foo"), IsOkAndHolds(RULES_ELISP_NATIVE_LITERAL("Foo")));
}

TEST(ToNativeTest, RejectsNonAscii) {
  if constexpr (kWindows) {
    EXPECT_THAT(ToNative("Foó"), StatusIs(absl::StatusCode::kInvalidArgument));
  }
}

}  // namespace rules_elisp
