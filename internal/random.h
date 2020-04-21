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

#ifndef PHST_RULES_ELISP_INTERNAL_RANDOM_H
#define PHST_RULES_ELISP_INTERNAL_RANDOM_H

#include <string>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#include "absl/random/random.h"
#include "absl/strings/string_view.h"
#pragma GCC diagnostic pop

namespace phst_rules_elisp {

class random {
 public:
  std::string temp_name(absl::string_view tmpl);

 private:
  absl::BitGen engine_;
};

}  // phst_rules_elisp

#endif
