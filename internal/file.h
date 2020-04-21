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

#include "internal/random.h"

namespace phst_rules_elisp {

enum class file_mode {
  read = O_RDONLY,
  write = O_WRONLY,
  readwrite = O_RDWR,
  create = O_CREAT,
  excl = O_EXCL,
};

inline file_mode operator|(const file_mode a, const file_mode b) {
  return static_cast<file_mode>(static_cast<int>(a) | static_cast<int>(b));
}

class file : public std::streambuf {
 public:
  explicit file(std::string path, file_mode mode);
  ~file() noexcept override;
  file(const file&) = delete;
  file& operator=(const file&) = delete;

  std::FILE* open_c_file(const char* mode);

  const std::string& path() const noexcept { return path_; }

  void close();

 protected:
  file();

  void open(std::string path, file_mode mode);

 private:
  virtual void do_close();
  int_type overflow(int_type ch = traits_type::eof()) final;
  std::streamsize xsputn(const char_type* data, std::streamsize count) final;
  std::size_t write(const char* data, std::size_t count);
  int_type underflow() final;
  std::streamsize xsgetn(char_type* data, std::streamsize count) final;
  std::size_t read(char* data, std::size_t count);
  void imbue(const std::locale& locale) final;
  [[noreturn]] file* setbuf(char*, std::streamsize) final;
  int sync() final;
  [[nodiscard]] bool flush();

  int fd_ = -1;
  std::array<char_type, 0x1000> get_;
  std::array<char_type, 0x1000> put_;
  std::string path_;
};

class temp_file : public file {
 public:
  explicit temp_file(const std::string& directory,
                     std::string_view tmpl, random& random);
  ~temp_file() noexcept override;
  temp_file(const temp_file&) = delete;
  temp_file& operator=(const temp_file&) = delete;

 private:
  void do_close() final;
  [[nodiscard]] std::error_code remove() noexcept;
};

template <typename T>
class basic_stream : public std::iostream {
 public:
  static_assert(std::is_base_of<file, T>::value,
                "basic_stream works only with file types");

  template <typename... As>
  explicit basic_stream(As&&... args) : file_(std::forward<As>(args)...) {
    this->init(&file_);
    this->exceptions(badbit | failbit | eofbit);
    this->imbue(std::locale::classic());
  }

  basic_stream(const basic_stream&) = delete;
  basic_stream& operator=(const basic_stream&) = delete;

  void close() { file_.close(); }
  const std::string& path() const noexcept { return file_.path(); }

 private:
  T file_;
};

using stream = basic_stream<file>;
using temp_stream = basic_stream<temp_file>;

inline constexpr std::string_view filename(std::string_view name) noexcept {
  const auto pos = name.rfind('/');
  return pos == name.npos ? name : name.substr(pos + 1);
}

inline constexpr std::string_view parent(std::string_view name) noexcept {
  const auto pos = name.rfind('/');
  return pos == name.npos ? std::string_view() : name.substr(0, pos);
}

constexpr std::string_view remove_slash(std::string_view name) noexcept {
  return (name.empty() || name.back() != '/') ? name
                                              : name.substr(0, name.size() - 1);
}

constexpr bool is_absolute(std::string_view name) noexcept {
  return !name.empty() && name.front() == '/';
}

std::string join_path(std::string_view a, std::string_view b);

template <typename... Ts>
std::string join_path(std::string_view a, std::string_view b, Ts&&... rest) {
  static_assert(sizeof...(Ts) > 0,
                "this overload should only be instantiated with at least three "
                "arguments");
  return join_path(join_path(a, b), std::forward<Ts>(rest)...);
}

std::string make_absolute(std::string_view name);

[[nodiscard]] bool file_exists(const std::string& name) noexcept;
[[nodiscard]] std::error_code remove_file(const std::string& name) noexcept;
[[nodiscard]] std::string temp_dir();

class directory {
 public:
  class iterator {
   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = std::string;
    using difference_type = void;
    using pointer = const std::string*;
    using reference = const std::string&;

    inline friend bool operator==(const iterator i, const iterator j) noexcept {
      return i.end() == j.end();
    }

    inline friend bool operator!=(const iterator i, const iterator j) noexcept {
      return !(i == j);
    }

    reference operator*() const noexcept { return entry_; }
    pointer operator->() const noexcept { return &entry_; }

    iterator& operator++() {
      this->advance();
      return *this;
    }

    iterator operator++(int) {
      const auto copy = *this;
      this->advance();
      return copy;
    }

   private:
    explicit iterator(DIR* const dir) : dir_(dir) { this->advance(); }
    explicit iterator() : dir_(nullptr) {}

    friend class directory;

    bool end() const noexcept { return dir_ == nullptr; }
    void advance();

    ::DIR* dir_;
    std::string entry_;
  };

  explicit directory(const std::string& name);
  ~directory() noexcept;

  [[nodiscard]] std::error_code close() noexcept;

  iterator begin() const { return iterator(dir_); }
  iterator end() const { return iterator(); }

 private:
  ::DIR* dir_;
};

}  // phst_rules_elisp

#endif
