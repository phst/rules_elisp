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

#include "internal/random.h"

#include <algorithm>
#include <array>
#include <functional>
#include <iomanip>
#include <limits>
#include <locale>
#include <random>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>

namespace phst_rules_elisp {

std::string random::temp_name(const std::string_view tmpl) {
  const auto pos = tmpl.rfind('*');
  if (pos == tmpl.npos) {
    throw std::invalid_argument("no * in template " + std::string(tmpl));
  }
  const auto prefix = tmpl.substr(0, pos);
  const auto suffix = tmpl.substr(pos + 1);
  std::uniform_int_distribution<unsigned long> distribution;
  using limits = std::numeric_limits<decltype(distribution)::result_type>;
  static_assert(limits::radix == 2);
  constexpr int bits_per_hex_digit = 4;
  constexpr int width =
      (limits::digits + bits_per_hex_digit - 1) / bits_per_hex_digit;
  std::ostringstream stream;
  stream.exceptions(std::ios::badbit | std::ios::failbit | std::ios::eofbit);
  stream.imbue(std::locale::classic());
  stream << prefix << std::setfill('0') << std::right << std::hex
         << std::setw(width) << distribution(engine_) << suffix;
  return stream.str();
}

random::engine random::init_engine() {
  std::random_device device;
  static_assert(engine::word_size == 32);
  std::array<decltype(device)::result_type, engine::state_size> seed;
  std::generate(seed.begin(), seed.end(), std::ref(device));
  std::seed_seq sequence(seed.cbegin(), seed.cend());
  return engine(sequence);
}

}  // phst_rules_elisp
