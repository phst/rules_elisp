// Copyright 2020-2025 Google LLC
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

#ifndef ELISP_PRIVATE_TOOLS_COPY_H_
#define ELISP_PRIVATE_TOOLS_COPY_H_

#include "absl/status/status.h"

#include "elisp/private/tools/system.h"

namespace rules_elisp {

// Copy files from a directory into another.  Both `from` and `to` name
// directories, and `list` names a text file containing newline-separated
// filenames to copy.  All the filenames in `list` must be within `from`, and
// they are interpreted as relative to `from`.
absl::Status CopyFiles(const FileName& from, const FileName& to, const FileName& list);

}  // namespace rules_elisp

#endif  // ELISP_PRIVATE_TOOLS_COPY_H_
