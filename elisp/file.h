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

#ifndef PHST_RULES_ELISP_ELISP_FILE_H
#define PHST_RULES_ELISP_ELISP_FILE_H

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <fcntl.h>

#include <initializer_list>
#include <string>
#include <utility>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#include "absl/base/attributes.h"
#include "absl/base/casts.h"
#include "absl/random/random.h"
#include "absl/status/status.h"
#include "absl/strings/string_view.h"
#include "absl/utility/utility.h"
#pragma GCC diagnostic pop

#include "elisp/status.h"

namespace phst_rules_elisp {

enum class FileMode {
  kRead = O_RDONLY,
  kWrite = O_WRONLY,
  kReadWrite = O_RDWR,
  kCreate = O_CREAT,
  kExclusive = O_EXCL,
};

inline FileMode operator|(const FileMode a, const FileMode b) {
  return static_cast<FileMode>(static_cast<int>(a) | static_cast<int>(b));
}

class File {
 public:
  static StatusOr<File> Open(std::string path, FileMode mode);
  ~File() noexcept;
  File(const File&) = delete;
  File(File&& other);
  File& operator=(const File&) = delete;
  File& operator=(File&& other);
  const std::string& path() const noexcept { return path_; }
  absl::Status Close();
  StatusOr<std::string> Read();
  absl::Status Write(absl::string_view data);

 private:
  explicit File(const int fd, std::string path)
      : fd_(fd), path_(std::move(path)) {}
  absl::Status Fail(absl::string_view function) const;

  int fd_ = -1;
  std::string path_;
};

class TempFile {
 public:
  static StatusOr<TempFile> Create(const std::string& directory,
                                   absl::string_view tmpl,
                                   absl::BitGen& random);

  ~TempFile() noexcept;
  TempFile(const TempFile&) = delete;
  TempFile(TempFile&&) = default;
  TempFile& operator=(const TempFile&) = delete;
  TempFile& operator=(TempFile&&) = default;
  const std::string& path() const noexcept { return file_.path(); }
  absl::Status Close();
  StatusOr<std::string> Read() { return file_.Read(); }
  absl::Status Write(const absl::string_view data) { return file_.Write(data); }

 private:
  explicit TempFile(File file) : file_(std::move(file)) {}

  File file_;
};

constexpr bool IsAbsolute(absl::string_view name) noexcept {
  return !name.empty() && name.front() == '/';
}

std::string JoinPathImpl(std::initializer_list<absl::string_view> pieces);

template <typename... Ts>
std::string JoinPath(Ts&&... pieces) {
  static_assert(sizeof...(pieces) >= 1, "need at least one piece to join");
  return JoinPathImpl({absl::implicit_cast<absl::string_view>(pieces)...});
}

StatusOr<std::string> MakeAbsolute(absl::string_view name);

ABSL_MUST_USE_RESULT bool FileExists(const std::string& name) noexcept;
absl::Status RemoveFile(const std::string& name) noexcept;
ABSL_MUST_USE_RESULT std::string TempDir();
ABSL_MUST_USE_RESULT std::string TempName(absl::string_view dir,
                                          absl::string_view tmpl,
                                          absl::BitGen& random);

class Directory {
 public:
  static StatusOr<Directory> Open(const std::string& name);
  Directory(const Directory&) = delete;
  Directory(Directory&& other) : dir_(absl::exchange(other.dir_, nullptr)) {}
  Directory& operator=(const Directory&) = delete;
  Directory& operator=(Directory&& other);
  ~Directory() noexcept;
  absl::Status Close() noexcept;
  StatusOr<std::string> Read();

 private:
  explicit Directory(::DIR* const dir) : dir_(dir) {}

  ::DIR* dir_;
};

}  // phst_rules_elisp

#endif
