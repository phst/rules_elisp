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

#ifndef ELISP_PRIVATE_TOOLS_SYSTEM_H_
#define ELISP_PRIVATE_TOOLS_SYSTEM_H_

#include <cstddef>
#include <string_view>
#include <system_error>
#include <tuple>
#include <utility>

#include "absl/container/flat_hash_map.h"
#include "absl/log/check.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_join.h"
#include "absl/types/span.h"

#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/strings.h"

namespace rules_elisp {

std::size_t MaxFilename();

absl::Status MakeErrorStatus(const std::error_code& code,
                             std::string_view function, std::string_view args);

template <typename... Ts>
absl::Status ErrorStatus(const std::error_code& code,
                         const std::string_view function, Ts&&... args) {
  return MakeErrorStatus(code, function,
                         absl::StrJoin(std::forward_as_tuple(args...), ", "));
}

[[nodiscard]] std::error_code WindowsError();

template <typename... Ts>
absl::Status WindowsStatus(const std::string_view function, Ts&&... args) {
  const std::error_code code = WindowsError();
  return ErrorStatus(code, function, std::forward<Ts>(args)...);
}

[[nodiscard]] std::error_code ErrnoError();

template <typename... Ts>
absl::Status ErrnoStatus(const std::string_view function, Ts&&... args) {
  const std::error_code code = ErrnoError();
  return ErrorStatus(code, function, std::forward<Ts>(args)...);
}

absl::StatusOr<NativeString> MakeAbsolute(NativeStringView file);

class Environment final {
 private:
  struct Hash {
    std::size_t operator()(NativeStringView string) const;
  };

  struct Equal {
    bool operator()(NativeStringView a, NativeStringView b) const;
  };

  using Map = absl::flat_hash_map<NativeString, NativeString, Hash, Equal>;

 public:
  static absl::StatusOr<Environment> Current();

  template <typename I>
  static absl::StatusOr<Environment> Create(I begin, const I end) {
    Map map;
    for (; begin != end; ++begin) {
      const auto& [key, value] = *begin;
      if (key.empty()) {
        return absl::InvalidArgumentError("Empty environment variable name");
      }
      const auto [it, ok] = map.emplace(key, value);
      if (!ok) {
        return absl::AlreadyExistsError(
            absl::StrCat("Duplicate environment variable ", Escape(key)));
      }
    }
    return Environment(std::move(map));
  }

  using value_type = Map::value_type;
  using reference = Map::reference;
  using const_reference = Map::const_reference;
  using iterator = Map::iterator;
  using const_iterator = Map::const_iterator;
  using difference_type = Map::difference_type;
  using size_type = Map::size_type;

  Environment() = default;
  Environment(const Environment&) = default;
  Environment& operator=(const Environment&) = default;
  Environment(Environment&&) = default;
  Environment& operator=(Environment&&) = default;

  iterator begin() { return map_.begin(); }
  const_iterator begin() const { return map_.begin(); }
  iterator end() { return map_.end(); }
  const_iterator end() const { return map_.end(); }
  const_iterator cbegin() const { return map_.cbegin(); }
  const_iterator cend() const { return map_.cend(); }
  friend bool operator==(const Environment& a, const Environment& b) {
    return a.map_ == b.map_;
  }
  friend bool operator!=(const Environment& a, const Environment& b) {
    return a.map_ != b.map_;
  }
  void swap(Environment& other) { map_.swap(other.map_); }
  size_type size() const { return map_.size(); }
  size_type max_size() const { return map_.max_size(); }
  bool empty() const { return map_.empty(); }

  template <std::size_t N>
  void Add(const NativeChar (&key)[N], NativeStringView value) {
    static_assert(N > 1, "empty environment variable");
    map_.emplace(key, value);
  }

  template <std::size_t N>
  void Remove(const NativeChar (&key)[N]) {
    static_assert(N > 1, "empty environment variable");
    map_.erase(key);
  }

  void Merge(const Environment& other) {
    map_.insert(other.begin(), other.end());
  }

 private:
  explicit Environment(Map map) : map_(std::move(map)) {}

  Map map_;
};

absl::StatusOr<int> Run(absl::Span<const NativeString> args,
                        const Environment& env);

class DosDevice final {
 public:
  static absl::StatusOr<DosDevice> Create(NativeStringView target);

  ~DosDevice() noexcept;

  DosDevice(const DosDevice&) = delete;
  DosDevice& operator=(const DosDevice&) = delete;

  DosDevice(DosDevice&& other)
      : name_(std::exchange(other.name_, NativeString())),
        target_(std::exchange(other.target_, NativeString())) {}

  DosDevice& operator=(DosDevice&& other) {
    name_ = std::exchange(other.name_, NativeString());
    target_ = std::exchange(other.target_, NativeString());
    return *this;
  }

  NativeString name() const { return name_; }

 private:
  explicit DosDevice(const NativeStringView name, const NativeStringView target)
      : name_(name), target_(target) {
    CHECK(!name.empty());
    CHECK(!target.empty());
  }

  NativeString name_;
  NativeString target_;
};

}  // namespace rules_elisp

#endif  // ELISP_PRIVATE_TOOLS_SYSTEM_H_
