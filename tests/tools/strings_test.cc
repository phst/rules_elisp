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

namespace rules_elisp {

using absl_testing::IsOk;
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

TEST(ContainsNullTest, AcceptsStringsWithoutEmbeddedNull) {
  EXPECT_FALSE(ContainsNull(""));
  EXPECT_FALSE(ContainsNull(L""));
  EXPECT_FALSE(ContainsNull("foo"));
  EXPECT_FALSE(ContainsNull(L"foo"));
}

TEST(ContainsNullTest, RejectsStringsWithEmbeddedNull) {
  EXPECT_TRUE(ContainsNull(View("\0")));
  EXPECT_TRUE(ContainsNull(View(L"\0")));
}

TEST(PercentEncode, EncodesToAscii) {
  EXPECT_EQ(PercentEncode(""), "");
  EXPECT_EQ(PercentEncode(View("Foó % \0\1\xFF")), "Fo%c3%b3 %25 %00%01%ff");
}

}  // namespace rules_elisp
