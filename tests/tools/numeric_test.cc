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

#include "elisp/private/tools/numeric.h"

#include <cstdint>
#include <limits>
#include <optional>

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace rules_elisp {

using ::testing::Optional;

TEST(InRangeTest, ChecksRange) {
  EXPECT_TRUE(InRange<int>(0));
  EXPECT_TRUE(InRange<int>(0u));
  EXPECT_TRUE(InRange<int>(0l));
  EXPECT_TRUE(InRange<int>(0ul));
  EXPECT_TRUE(InRange<int>(std::numeric_limits<int>::min()));
  EXPECT_TRUE(InRange<int>(std::numeric_limits<int>::max()));
  EXPECT_TRUE(InRange<int>(std::numeric_limits<unsigned int>::min()));
  EXPECT_FALSE(InRange<int>(std::numeric_limits<unsigned int>::max()));

  EXPECT_TRUE(InRange<unsigned int>(0));
  EXPECT_TRUE(InRange<unsigned int>(0u));
  EXPECT_TRUE(InRange<unsigned int>(0l));
  EXPECT_TRUE(InRange<unsigned int>(0ul));
  EXPECT_FALSE(InRange<unsigned int>(-1));
  EXPECT_TRUE(InRange<unsigned int>(std::numeric_limits<unsigned int>::min()));
  EXPECT_TRUE(InRange<unsigned int>(std::numeric_limits<unsigned int>::max()));
  EXPECT_FALSE(InRange<unsigned int>(std::numeric_limits<int>::min()));
  EXPECT_TRUE(InRange<unsigned int>(std::numeric_limits<int>::max()));

  EXPECT_TRUE(InRange<std::int8_t>(0));
  EXPECT_TRUE(InRange<std::int8_t>(0u));
  EXPECT_TRUE(InRange<std::int8_t>(0l));
  EXPECT_TRUE(InRange<std::int8_t>(0ul));
  EXPECT_TRUE(InRange<std::int8_t>(-128));
  EXPECT_FALSE(InRange<std::int8_t>(-129));
  EXPECT_TRUE(InRange<std::int8_t>(127));
  EXPECT_FALSE(InRange<std::int8_t>(128));

  EXPECT_TRUE(InRange<std::uint8_t>(0));
  EXPECT_TRUE(InRange<std::uint8_t>(0u));
  EXPECT_TRUE(InRange<std::uint8_t>(0l));
  EXPECT_TRUE(InRange<std::uint8_t>(0ul));
  EXPECT_FALSE(InRange<std::uint8_t>(-1));
  EXPECT_TRUE(InRange<std::uint8_t>(255));
  EXPECT_FALSE(InRange<std::uint8_t>(256));
}

TEST(CastNumberTest, ReturnsValueIfInRange) {
  EXPECT_THAT(CastNumber<unsigned int>(0), Optional(0u));
}

TEST(CastNumberTest, ReturnsNulloptIfOutOfRange) {
  EXPECT_EQ(CastNumber<unsigned int>(-1), std::nullopt);
}

}  // namespace rules_elisp
