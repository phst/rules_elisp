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
#include <cstdint>
#include <cstdio>
#include <optional>
#include <string>
#include <string_view>
#include <system_error>
#include <tuple>
#include <utility>
#include <vector>

#include "absl/base/attributes.h"
#include "absl/base/nullability.h"
#include "absl/container/flat_hash_map.h"
#include "absl/log/check.h"
#include "absl/log/die_if_null.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"
#include "absl/strings/str_join.h"
#include "absl/time/time.h"
#include "absl/types/span.h"

#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/strings.h"

namespace rules_elisp {

enum class Encoding { kAscii, kUtf8 };

// Convert a native string to a std::string.  On Windows, interpret the string
// as UTF-16, and convert it to a byte string using the specified encoding.  On
// POSIX systems, ignore the encoding and return the string unchanged.
absl::StatusOr<std::string> ToNarrow(NativeStringView string,
                                     Encoding encoding);

// Convert a narrow string to a native string.  On Windows, interpret the string
// using the given encoding, and convert to UTF-16.  On POSIX systems, ignore
// the encoding and return the string unchanged.
absl::StatusOr<NativeString> ToNative(std::string_view string,
                                      Encoding encoding);

class FileName final {
 public:
  static absl::StatusOr<FileName> FromString(NativeStringView string);

  FileName(const FileName&) = default;
  FileName& operator=(const FileName&) = default;
  FileName(FileName&&) = default;
  FileName& operator=(FileName&&) = default;

  const NativeString& string() const ABSL_ATTRIBUTE_LIFETIME_BOUND {
    CHECK(!string_.empty());
    return string_;
  }

  NativeChar* absl_nonnull pointer() ABSL_ATTRIBUTE_LIFETIME_BOUND {
    CHECK(!string_.empty());
    CHECK(!ContainsNull(string_));
    return string_.data();
  }

  const NativeChar* absl_nonnull pointer() const ABSL_ATTRIBUTE_LIFETIME_BOUND {
    CHECK(!string_.empty());
    CHECK(!ContainsNull(string_));
    return string_.data();
  }

  absl::StatusOr<FileName> Parent() const;
  absl::StatusOr<FileName> Child(const FileName& child) const;
  absl::StatusOr<FileName> Child(NativeStringView child) const;
  absl::StatusOr<FileName> Join(const FileName& descendant) const;
  absl::StatusOr<FileName> Join(NativeStringView descendant) const;

  bool IsAbsolute() const;
  absl::StatusOr<FileName> MakeAbsolute() const;
  absl::StatusOr<FileName> MakeRelative(const FileName& base) const;

  friend bool operator==(const FileName& a, const FileName& b) {
    return a.string_ == b.string_;
  }

  friend bool operator!=(const FileName& a, const FileName& b) {
    return a.string_ != b.string_;
  }

  friend bool operator<(const FileName& a, const FileName& b) {
    return a.string_ < b.string_;
  }

  friend bool operator>(const FileName& a, const FileName& b) {
    return a.string_ > b.string_;
  }

  friend bool operator<=(const FileName& a, const FileName& b) {
    return a.string_ <= b.string_;
  }

  friend bool operator>=(const FileName& a, const FileName& b) {
    return a.string_ >= b.string_;
  }

  // https://abseil.io/docs/cpp/guides/abslstringify#basic-usage
  template <typename Sink>
  friend void AbslStringify(Sink& sink, const FileName& name) {
    absl::Format(&sink, "%s", name.string_);
  }

 private:
  explicit FileName(NativeString string) : string_(std::move(string)) {
    CHECK(!string_.empty());
  }

  NativeString string_;
};

absl::Status MakeErrorStatus(const std::error_code& code,
                             std::string_view function, std::string_view args);

struct Ellipsis final {};
inline constexpr Ellipsis kEllipsis;

struct Oct final {
  std::uint64_t value;
  explicit constexpr Oct(const std::uint64_t v) : value(v) {}
};

struct ArgFormatter final {
  void operator()(std::string* const absl_nonnull out,
                  const absl::AlphaNum& value) const {
    const auto base = absl::AlphaNumFormatter();
    base(out, value);
  }

  void operator()(std::string* const absl_nonnull out, Ellipsis) const {
    out->append("...");
  }

  void operator()(std::string* const absl_nonnull out,
                  const std::string& string) const {
    out->append(Quote(string));
  }

  void operator()(std::string* const absl_nonnull out,
                  const std::wstring& string) const {
    out->append(Quote(string));
  }

  void operator()(std::string* const absl_nonnull out,
                  const std::string_view string) const {
    out->append(Quote(string));
  }

  void operator()(std::string* const absl_nonnull out,
                  const std::wstring_view string) const {
    out->append(Quote(string));
  }

  void operator()(std::string* const absl_nonnull out,
                  const char* const absl_nullable string) const {
    out->append(string == nullptr ? "nullptr" : Quote(string));
  }

  void operator()(std::string* const absl_nonnull out,
                  const wchar_t* const absl_nullable string) const {
    out->append(string == nullptr ? "nullptr" : Quote(string));
  }

  void operator()(std::string* const absl_nonnull out,
                  const FileName& file) const {
    out->append(Quote(file.string()));
  }

  void operator()(std::string* const absl_nonnull out, std::nullptr_t) const {
    out->append("nullptr");
  }

  void operator()(std::string* const absl_nonnull out,
                  const absl::Hex& number) const {
    const auto base = absl::AlphaNumFormatter();
    out->append("0x");
    base(out, number);
  }

  void operator()(std::string* const absl_nonnull out, const Oct number) const {
    absl::StrAppendFormat(out, "0%03o", number.value);
  }
};

template <typename... Ts>
absl::Status ErrorStatus(const std::error_code& code,
                         const std::string_view function, Ts&&... args) {
  const ArgFormatter formatter;
  return MakeErrorStatus(
      code, function,
      absl::StrJoin(std::forward_as_tuple(args...), ", ", formatter));
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

absl::StatusOr<std::string> ReadFile(const FileName& file);
absl::Status WriteFile(const FileName& file, std::string_view contents);
[[nodiscard]] bool FileExists(const FileName& file);
[[nodiscard]] bool IsNonEmptyDirectory(const FileName& directory);
absl::Status Unlink(const FileName& file);
absl::Status CreateDirectory(const FileName& name);
absl::Status CreateDirectories(const FileName& name);
absl::Status RemoveDirectory(const FileName& name);
absl::Status RemoveTree(const FileName& directory);
absl::StatusOr<std::vector<FileName>> ListDirectory(const FileName& dir);
absl::Status CopyFile(const FileName& from, const FileName& to);
absl::Status CopyTree(const FileName& from, const FileName& to);

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
      if (ContainsNull(key)) {
        return absl::InvalidArgumentError(absl::StrFormat(
            "Environment variable name %s contains null character", key));
      }
      if (ContainsNull(value)) {
        return absl::InvalidArgumentError(absl::StrFormat(
            "Value %s for environment variable %s contains null character",
            value, key));
      }
      const auto [it, ok] = map.emplace(key, value);
      if (!ok) {
        return absl::AlreadyExistsError(
            absl::StrFormat("Duplicate environment variable %s", key));
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
  NativeStringView Get(const NativeChar (&key)[N]) const
      ABSL_ATTRIBUTE_LIFETIME_BOUND {
    static_assert(N > 1, "empty environment variable");
    const auto it = map_.find(key);
    return it == map_.end() ? NativeStringView() : it->second;
  }

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

class TemporaryFile final {
 public:
  static absl::StatusOr<TemporaryFile> Create();

  TemporaryFile(const TemporaryFile&) = delete;
  TemporaryFile& operator=(const TemporaryFile&) = delete;

  TemporaryFile(TemporaryFile&& other)
      : name_(std::exchange(other.name_, std::nullopt)),
        file_(std::exchange(other.file_, nullptr)) {}

  TemporaryFile& operator=(TemporaryFile&& other) {
    name_ = std::exchange(other.name_, std::nullopt);
    file_ = std::exchange(other.file_, nullptr);
    return *this;
  }

  ~TemporaryFile() noexcept;

  const FileName& name() const ABSL_ATTRIBUTE_LIFETIME_BOUND {
    return name_.value();
  }

  absl::Status Write(std::string_view contents);

 private:
  explicit TemporaryFile(FileName name, std::FILE* const absl_nonnull file)
      : name_(std::move(name)), file_(ABSL_DIE_IF_NULL(file)) {}

  std::optional<FileName> name_;
  std::FILE* absl_nullable file_;
};

struct RunOptions final {
  // If set, change to this directory in the subprocess.
  std::optional<FileName> directory;

  // If set, redirect standard output and error to this file.
  std::optional<FileName> output_file;

  absl::Time deadline = absl::InfiniteFuture();
};

absl::StatusOr<int> Run(const FileName& program,
                        absl::Span<const NativeString> args,
                        const Environment& env, const RunOptions& options = {});

class DosDevice final {
 public:
  static absl::StatusOr<DosDevice> Create(const FileName& target);

  ~DosDevice() noexcept;

  DosDevice(const DosDevice&) = delete;
  DosDevice& operator=(const DosDevice&) = delete;

  DosDevice(DosDevice&& other)
      : name_(std::exchange(other.name_, NativeString())),
        target_(std::exchange(other.target_, std::nullopt)) {}

  DosDevice& operator=(DosDevice&& other) {
    name_ = std::exchange(other.name_, NativeString());
    target_ = std::exchange(other.target_, std::nullopt);
    return *this;
  }

  const NativeString& name() const ABSL_ATTRIBUTE_LIFETIME_BOUND {
    CHECK(!name_.empty());
    return name_;
  }

 private:
  explicit DosDevice(NativeString name, FileName target)
      : name_(std::move(name)), target_(std::move(target)) {
    CHECK(!name_.empty());
  }

  NativeString name_;
  std::optional<FileName> target_;
};

}  // namespace rules_elisp

#endif  // ELISP_PRIVATE_TOOLS_SYSTEM_H_
