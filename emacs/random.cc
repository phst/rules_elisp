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

#include "emacs/random.h"

#include "absl/random/random.h"
#include "absl/memory/memory.h"
#include "absl/strings/str_cat.h"

namespace phst_rules_elisp {

struct random::impl {
  absl::BitGen gen;
  absl::uniform_int_distribution<unsigned long> dist;
};

random::random() : impl_(absl::make_unique<impl>()) {}
random::~random() {}

std::string random::temp_name() {
  return absl::StrCat("tmp", absl::Hex(impl_->dist(impl_->gen)));
}

}  // namespace phst_rules_elisp
