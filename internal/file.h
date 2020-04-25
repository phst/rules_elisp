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

#ifndef PHST_RULES_ELISP_INTERNAL_FILE_H
#define PHST_RULES_ELISP_INTERNAL_FILE_H

#include <sys/types.h>
#include <dirent.h>
#include <fcntl.h>

#include <array>
#include <iosfwd>
#include <istream>
#include <iterator>
#include <locale>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>

#include "absl/base/attributes.h"
#include "absl/strings/string_view.h"

#include "internal/random.h"

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

class File : public std::streambuf {
 public:
  explicit File(std::string path, FileMode mode);
  ~File() noexcept override;
  File(const File&) = delete;
  File(File&& other);
  File& operator=(const File&) = delete;
  File& operator=(File&& other);

  std::FILE* OpenCFile(const char* mode);

  const std::string& path() const noexcept { return path_; }

  void Close();

 protected:
  File();

  void Open(std::string path, FileMode mode);

 private:
  struct ABSL_MUST_USE_RESULT Result {
    std::streamsize count;
    int error;
  };

  virtual void DoClose();
  void Move(File&& other);
  int_type overflow(int_type ch = traits_type::eof()) final;
  std::streamsize xsputn(const char_type* data, std::streamsize count) final;
  Result Write(const char* data, std::streamsize count);
  int_type underflow() final;
  std::streamsize xsgetn(char_type* data, std::streamsize count) final;
  Result Read(char* data, std::streamsize count);
  int sync() final;
  ABSL_MUST_USE_RESULT bool Flush();

  int fd_ = -1;
  std::array<char_type, 0x1000> get_;
  std::array<char_type, 0x1000> put_;
  std::string path_;
};

class TempFile : public File {
 public:
  explicit TempFile(const std::string& directory, absl::string_view tmpl,
                    Random& random);
  ~TempFile() noexcept override;
  TempFile(const TempFile&) = delete;
  TempFile(TempFile&&) = default;
  TempFile& operator=(const TempFile&) = delete;
  TempFile& operator=(TempFile&&) = default;

 private:
  void DoClose() final;
  ABSL_MUST_USE_RESULT std::error_code Remove() noexcept;
};

template <typename T>
class BasicStream : public std::iostream {
 public:
  static_assert(std::is_base_of<File, T>::value,
                "basic_stream works only with file types");

  template <typename... As>
  explicit BasicStream(As&&... args) : file_(std::forward<As>(args)...) {
    this->init(&file_);
    this->exceptions(badbit | failbit | eofbit);
    this->imbue(std::locale::classic());
  }

  BasicStream(const BasicStream&) = delete;
  BasicStream& operator=(const BasicStream&) = delete;

  BasicStream(BasicStream&& other)
      : std::iostream(std::move(other)), file_(std::move(other.file_)) {
    this->set_rdbuf(&file_);
    other.set_rdbuf(nullptr);
  }

  BasicStream& operator=(BasicStream&& other) {
    this->std::iostream::operator=(other);
    file_ = other.file;
    other.file_ = nullptr;
    this->set_rdbuf(file_);
    other.set_rdbuf(nullptr);
  }

  void Close() { file_.Close(); }
  const std::string& path() const noexcept { return file_.path(); }

 private:
  T file_;
};

using Stream = BasicStream<File>;
using TempStream = BasicStream<TempFile>;

inline absl::string_view FileName(absl::string_view name) noexcept {
  const auto pos = name.rfind('/');
  return pos == name.npos ? name : name.substr(pos + 1);
}

inline absl::string_view Parent(absl::string_view name) noexcept {
  const auto pos = name.rfind('/');
  return pos == name.npos ? absl::string_view() : name.substr(0, pos);
}

constexpr absl::string_view RemoveSlash(absl::string_view name) noexcept {
  return (name.empty() || name.back() != '/') ? name
                                              : name.substr(0, name.size() - 1);
}

constexpr bool IsAbsolute(absl::string_view name) noexcept {
  return !name.empty() && name.front() == '/';
}

std::string JoinPath(absl::string_view a, absl::string_view b);

template <typename... Ts>
std::string JoinPath(absl::string_view a, absl::string_view b, Ts&&... rest) {
  static_assert(sizeof...(Ts) > 0,
                "this overload should only be instantiated with at least three "
                "arguments");
  return JoinPath(JoinPath(a, b), std::forward<Ts>(rest)...);
}

std::string MakeAbsolute(absl::string_view name);

ABSL_MUST_USE_RESULT bool FileExists(const std::string& name) noexcept;
ABSL_MUST_USE_RESULT std::error_code RemoveFile(
    const std::string& name) noexcept;
ABSL_MUST_USE_RESULT std::string TempDir();

class Directory {
 public:
  class Iterator {
   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = std::string;
    using difference_type = void;
    using pointer = const std::string*;
    using reference = const std::string&;

    inline friend bool operator==(const Iterator i, const Iterator j) noexcept {
      return i.End() == j.End();
    }

    inline friend bool operator!=(const Iterator i, const Iterator j) noexcept {
      return !(i == j);
    }

    reference operator*() const noexcept { return entry_; }
    pointer operator->() const noexcept { return &entry_; }

    Iterator& operator++() {
      this->Advance();
      return *this;
    }

    Iterator operator++(int) {
      const auto copy = *this;
      this->Advance();
      return copy;
    }

   private:
    explicit Iterator(DIR* const dir) : dir_(dir) { this->Advance(); }
    explicit Iterator() : dir_(nullptr) {}

    friend class Directory;

    bool End() const noexcept { return dir_ == nullptr; }
    void Advance();

    ::DIR* dir_;
    std::string entry_;
  };

  explicit Directory(const std::string& name);
  ~Directory() noexcept;

  ABSL_MUST_USE_RESULT std::error_code Close() noexcept;

  Iterator begin() const { return Iterator(dir_); }
  Iterator end() const { return Iterator(); }

 private:
  ::DIR* dir_;
};

}  // phst_rules_elisp

#endif
