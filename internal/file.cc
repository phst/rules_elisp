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

#include "internal/file.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

#include <algorithm>
#include <cassert>
#include <cstdio>
#include <exception>
#include <filesystem>
#include <ios>
#include <iostream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>

#include "internal/random.h"

namespace phst_rules_elisp {

namespace fs = std::filesystem;

file::file() { this->setp(put_.data(), put_.data() + put_.size()); }

file::file(fs::path path, const file_mode mode) : file() {
  this->open(std::move(path), mode);
}

file::~file() noexcept {
  // We require calling close() explicitly.
  if (fd_ < 0) return;
  std::clog << "file " << path_ << " still open" << std::endl;
  std::terminate();
}

void file::open(fs::path path, const file_mode mode) {
  const int fd = ::open(path.c_str(), static_cast<int>(mode) | O_CLOEXEC,
                        S_IRUSR | S_IWUSR);
  if (fd < 0) {
    throw std::system_error(errno, std::generic_category(),
                            "open(" + path.string() + ')');
  }
  fd_ = fd;
  path_ = std::move(path);
}

std::FILE* file::open_c_file(const char* const mode) {
  const auto file = fdopen(fd_, mode);
  if (file == nullptr) {
    throw std::system_error(errno, std::generic_category(), "fdopen");
  }
  return file;
}

void file::close() {
  if (fd_ < 0) return;
  if (!this->flush()) {
    throw std::ios::failure("error closing file " + path_.string());
  }
  const auto status = ::close(fd_);
  fd_ = -1;
  if (status != 0) {
    throw std::system_error(errno, std::generic_category(), "close");
  }
  this->do_close();
  path_.clear();
}

void file::do_close() {}

file::int_type file::overflow(const int_type ch) {
  assert(this->pptr() == this->epptr());
  if (!this->flush()) return traits_type::eof();
  if (traits_type::eq_int_type(ch, traits_type::eof())) {
    return traits_type::not_eof(0);
  }
  traits_type::assign(put_.front(), traits_type::to_char_type(ch));
  this->pbump(1);
  return ch;
}

std::streamsize file::xsputn(const char_type* const data,
                             const std::streamsize count) {
  if (!this->flush()) return 0;
  return this->write(data, count);
}

std::size_t file::write(const char* data, std::size_t count) {
  std::size_t written = 0;
  while (count > 0) {
    const auto n = ::write(fd_, data, count);
    if (n < 0) throw std::system_error(errno, std::generic_category(), "write");
    if (n == 0) break;
    written += n;
    data += n;
    count -= n;
  }
  return written;
}

file::int_type file::underflow() {
  assert(this->gptr() == this->egptr());
  const auto read = this->read(get_.data(), get_.size());
  this->setg(get_.data(), get_.data(), get_.data() + read);
  if (read == 0) return traits_type::eof();
  assert(this->gptr() != nullptr);
  assert(this->gptr() != this->egptr());
  return traits_type::to_int_type(*this->gptr());
}

std::streamsize file::xsgetn(char_type* data, std::streamsize count) {
  if (count == 0) return 0;
  std::streamsize read = 0;
  if (this->gptr() != nullptr && this->egptr() != this->gptr()) {
    read = std::min(count, this->egptr() - this->gptr());
    traits_type::copy(data, this->gptr(), read);
    data += read;
    count -= read;
    this->setg(this->gptr(), this->gptr() + read, this->egptr());
  }
  return read + this->read(data, count);
}

std::size_t file::read(char* data, std::size_t count) {
  std::size_t read = 0;
  while (count > 0) {
    const auto n = ::read(fd_, data, count);
    if (n < 0) throw std::system_error(errno, std::generic_category(), "read");
    if (n == 0) break;
    read += n;
    data += n;
    count -= n;
  }
  return read;
}

void file::imbue(const std::locale& locale) {
  if (locale != std::locale::classic()) {
    throw std::logic_error("this class only supports the C locale");
  }
}

[[noreturn]] file* file::setbuf(char* const, const std::streamsize) {
  throw std::logic_error("this class doesn’t support changing the buffer");
}

int file::sync() {
  if (!this->flush()) return -1;
  if (::fsync(fd_) < 0) {
    throw std::system_error(errno, std::generic_category(), "fsync");
  }
  return 0;
}

[[nodiscard]] bool file::flush() {
  assert(this->pbase() != nullptr);
  assert(this->pptr() != nullptr);
  const auto signed_count = this->pptr() - this->pbase();
  assert(signed_count >= 0);
  const auto count = static_cast<std::size_t>(signed_count);
  const auto written = this->write(this->pbase(), count);
  assert(written <= count);
  if (written < count) return false;
  this->setp(put_.data(), put_.data() + put_.size());
  return true;
}

temp_file::temp_file(const fs::path& directory, const std::string_view tmpl,
                     random& random) {
  for (int i = 0; i < 10; i++) {
    auto name = directory / random.temp_name(tmpl);
    if (!fs::exists(name)) {
      this->open(std::move(name),
                 file_mode::readwrite | file_mode::create | file_mode::excl);
      return;
    }
  }
  throw std::runtime_error("can’t create temporary file in directory " +
                           directory.string() + " with template " +
                           std::string(tmpl));
}

temp_file::~temp_file() noexcept {
  const auto& path = this->path();
  std::error_code code;
  // Only print an error if removing the file failed (“false” return value), but
  // the file wasn’t already removed before (zero error code).
  if (!std::filesystem::remove(path, code) && code) {
    std::clog << "error removing temporary file " << path << ": " << code
              << ": " << code.message() << std::endl;
  }
}

void temp_file::do_close() { std::filesystem::remove(this->path()); }

}  // phst_rules_elisp
