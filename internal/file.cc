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

file::file(fs::path path, const char* const mode) : file() {
  this->open(std::move(path), mode);
}

file::~file() noexcept {
  // We require calling close() explicitly.
  if (file_ == nullptr) return;
  std::clog << "file " << path_ << " still open" << std::endl;
  std::terminate();
}

void file::open(fs::path path, const char* const mode) {
  const auto file = std::fopen(path.c_str(), mode);
  if (file == nullptr) {
    throw std::ios::failure("error opening file " + path.string() +
                            " in mode " + mode);
  }
  file_ = file;
  path_ = std::move(path);
  this->check();
}

void file::close() {
  if (file_ == nullptr) return;
  this->check();
  const auto status = std::fclose(file_);
  file_ = nullptr;
  if (status != 0) {
    throw std::ios::failure("error closing file " + path_.string());
  }
  path_.clear();
  this->do_close();
}

void file::do_close() {}

file::int_type file::overflow(const int_type ch) {
  assert(this->pptr() == this->epptr());
  if (!this->write()) return traits_type::eof();
  if (traits_type::eq_int_type(ch, traits_type::eof())) {
    return traits_type::not_eof(0);
  }
  traits_type::assign(put_.front(), traits_type::to_char_type(ch));
  this->pbump(1);
  return ch;
}

std::streamsize file::xsputn(const char_type* const data,
                             const std::streamsize count) {
  this->check();
  const auto written = std::fwrite(data, 1, count, file_);
  this->check();
  return written;
}

file::int_type file::underflow() {
  assert(this->gptr() == this->egptr());
  this->check();
  const auto read = std::fread(get_.data(), 1, get_.size(), file_);
  this->setg(get_.data(), get_.data(), get_.data() + read);
  this->check();
  if (read == 0) return traits_type::eof();
  assert(this->gptr() != nullptr);
  assert(this->gptr() != this->egptr());
  return traits_type::to_int_type(*this->gptr());
}

std::streamsize file::xsgetn(char_type* const data, std::streamsize count) {
  this->check();
  if (count == 0) return 0;
  std::streamsize read = 0;
  if (this->gptr() != nullptr && this->egptr() != this->gptr()) {
    read = std::min(count, this->egptr() - this->gptr());
    traits_type::copy(data, this->gptr(), read);
    count -= read;
    this->setg(this->gptr(), this->gptr() + read, this->egptr());
  }
  if (count == 0) return read;
  read += std::fread(data, 1, count, file_);
  this->check();
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
  if (!this->write()) return -1;
  this->check();
  return std::fflush(file_) == 0 ? 0 : -1;
}

[[nodiscard]] bool file::write() {
  assert(this->pbase() != nullptr);
  assert(this->pptr() != nullptr);
  this->check();
  const auto count = this->pptr() - this->pbase();
  assert(count >= 0);
  const auto written = std::fwrite(this->pbase(), 1, count, file_);
  this->check();
  assert(written <= static_cast<std::size_t>(count));
  if (written < static_cast<std::size_t>(count)) return false;
  this->setp(put_.data(), put_.data() + put_.size());
  return true;
}

void file::check() {
  if (file_ == nullptr) {
    throw std::logic_error("file " + path_.string() + " is already closed");
  }
  if (std::ferror(file_) != 0) {
    throw std::ios::failure("error in file " + path_.string());
  }
}

temp_file::temp_file(const fs::path& directory, const std::string_view tmpl,
                     random& random) {
  for (int i = 0; i < 10; i++) {
    auto name = directory / random.temp_name(tmpl);
    if (!fs::exists(name)) {
      this->open(std::move(name), "w+bx");
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
