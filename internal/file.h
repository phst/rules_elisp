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

#include <fcntl.h>

#include <array>
#include <filesystem>
#include <iosfwd>
#include <locale>
#include <string_view>
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
  explicit file(std::filesystem::path path, file_mode mode);
  ~file() noexcept override;
  file(const file&) = delete;
  file& operator=(const file&) = delete;

  std::FILE* open_c_file(const char* mode);

  const std::filesystem::path& path() const noexcept { return path_; }

  void close();

 protected:
  file();

  void open(std::filesystem::path path, file_mode mode);

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
  std::filesystem::path path_;
};

class temp_file : public file {
 public:
  explicit temp_file(const std::filesystem::path& directory,
                     std::string_view tmpl, random& random);
  ~temp_file() noexcept override;
  temp_file(const temp_file&) = delete;
  temp_file& operator=(const temp_file&) = delete;

 private:
  void do_close() final;
};

template <typename T>
class basic_stream : public std::iostream {
 public:
  static_assert(std::is_base_of_v<file, T>);

  template <typename... As>
  explicit basic_stream(As&&... args) : file_(std::forward<As>(args)...) {
    this->init(&file_);
    this->exceptions(badbit | failbit | eofbit);
    this->imbue(std::locale::classic());
  }

  basic_stream(const basic_stream&) = delete;
  basic_stream& operator=(const basic_stream&) = delete;

  void close() { file_.close(); }
  const std::filesystem::path& path() const noexcept { return file_.path(); }

 private:
  T file_;
};

using stream = basic_stream<file>;
using temp_stream = basic_stream<temp_file>;

}  // phst_rules_elisp

#endif
