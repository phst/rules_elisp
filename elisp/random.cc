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

#include "elisp/random.h"

#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <random>
#include <string>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#include "absl/strings/string_view.h"
#include "absl/strings/str_cat.h"
#pragma GCC diagnostic pop

namespace phst_rules_elisp {

std::string Random::TempName(const absl::string_view tmpl) {
  const auto pos = tmpl.rfind('*');
  if (pos == tmpl.npos) {
    std::clog << "no * in template " << tmpl << std::endl;
    std::abort();
  }
  const auto prefix = tmpl.substr(0, pos);
  const auto suffix = tmpl.substr(pos + 1);
  std::uniform_int_distribution<std::uint64_t> distribution;
  return absl::StrCat(
      prefix, absl::Hex(distribution(engine_), absl::kZeroPad16), suffix);
}

}  // phst_rules_elisp
