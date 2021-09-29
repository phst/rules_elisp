// Copyright 2020, 2021 Google LLC
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

#include "elisp/file.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <cassert>
#include <cstdlib>
#include <initializer_list>
#include <iostream>
#include <memory>
#include <string>
#include <type_traits>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#include "absl/base/attributes.h"
#include "absl/meta/type_traits.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "absl/strings/string_view.h"
#include "absl/strings/strip.h"
#pragma GCC diagnostic pop

#include "elisp/status.h"
#include "elisp/str.h"

namespace phst_rules_elisp {

namespace {

template <typename T>
constexpr absl::make_unsigned_t<T> ToUnsigned(const T n) noexcept {
  static_assert(std::is_integral<T>::value, "type is not integral");
  return static_cast<absl::make_unsigned_t<T>>(n);
}

}  // namespace

std::string JoinPathImpl(const std::initializer_list<absl::string_view> pieces) {
  assert(pieces.begin() < pieces.end());
  const auto format = [](std::string* const out, absl::string_view name) {
    absl::ConsumePrefix(&name, "/");
    absl::ConsumeSuffix(&name, "/");
    out->append(name.begin(), name.end());
  };
  // Make sure to not strip leading or trailing slashes.
  const auto first = *pieces.begin();
  const auto last = *(pieces.end() - 1);
  return absl::StrCat((first.length() > 1 && first.front() == '/') ? "/" : "",
                      absl::StrJoin(pieces, "/", format),
                      (last.length() > 1 && last.back() == '/') ? "/" : "");
}

absl::StatusOr<std::string> MakeAbsolute(const absl::string_view name) {
  if (IsAbsolute(name)) return std::string(name);
  struct free {
    void operator()(void* const ptr) const noexcept { std::free(ptr); }
  };
  const std::unique_ptr<char, free> dir(::getcwd(nullptr, 0));
  if (dir == nullptr) return ErrnoStatus("getcwd");
  return JoinPath(dir.get(), name);
}

absl::StatusOr<std::string> MakeRelative(const absl::string_view name,
                                         std::string root) {
  ASSIGN_OR_RETURN(const auto abs, MakeAbsolute(name));
  assert(!root.empty());
  if (root.back() != '/') root += '/';
  absl::string_view result = abs;
  absl::ConsumePrefix(&result, root);
  return std::string(result);
}

ABSL_MUST_USE_RESULT bool FileExists(const std::string& name) noexcept {
  struct stat info;
  return ::lstat(Pointer(name), &info) == 0;
}

absl::Status RemoveFile(const std::string& name) noexcept {
  return ::unlink(Pointer(name)) == 0 ? absl::OkStatus()
                                      : ErrnoStatus("unlink", name);
}

}  // phst_rules_elisp
