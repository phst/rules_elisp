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
#include <cerrno>
#include <cstddef>
#include <cstdlib>
#include <ios>
#include <iostream>
#include <limits>
#include <memory>
#include <stdexcept>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>

#include "absl/strings/string_view.h"

#include "internal/int.h"
#include "internal/random.h"

namespace phst_rules_elisp {

file::file() { this->setp(put_.data(), put_.data() + put_.size()); }

file::file(std::string path, const file_mode mode) : file() {
  this->open(std::move(path), mode);
}

file::file(file&& other)
    : std::streambuf(other),
      fd_(other.fd_),
      get_(other.get_),
      put_(other.put_),
      path_(std::move(other.path_)) {
  this->move(std::move(other));
}

file& file::operator=(file&& other) {
  this->std::streambuf::operator=(other);
  fd_ = other.fd_;
  get_ = other.get_;
  put_ = other.put_;
  path_ = std::move(other.path_);
  this->move(std::move(other));
  return *this;
}

file::~file() noexcept {
  if (fd_ >= 0) std::clog << "file " << path_ << " still open" << std::endl;
}

void file::open(std::string path, const file_mode mode) {
  int fd = -1;
  while (true) {
    fd = ::open(path.c_str(), static_cast<int>(mode) | O_CLOEXEC,
                S_IRUSR | S_IWUSR);
    if (fd >= 0 || errno != EINTR) break;
  }
  if (fd < 0) {
    throw std::system_error(errno, std::system_category(),
                            "open(" + path + ')');
  }
  fd_ = fd;
  path_ = std::move(path);
}

std::FILE* file::open_c_file(const char* const mode) {
  const auto file = fdopen(fd_, mode);
  if (file == nullptr) {
    throw std::system_error(errno, std::system_category(), "fdopen");
  }
  return file;
}

void file::close() {
  if (fd_ < 0) return;
  if (!this->flush()) {
    throw std::ios::failure("error closing file " + path_);
  }
  int status = -1;
  while (true) {
    status = ::close(fd_);
    if (status == 0 || errno != EINTR) break;
  }
  if (status != 0) {
    throw std::system_error(errno, std::system_category(), "close");
  }
  fd_ = -1;
  this->do_close();
  path_.clear();
}

void file::do_close() {}

void file::move(file&& other) {
  other.fd_ = -1;
  other.path_.clear();
  if (other.eback() != nullptr) {
    this->setg(get_.data() + (other.eback() - other.get_.data()),
               get_.data() + (other.gptr() - other.get_.data()),
               get_.data() + (other.egptr() - other.get_.data()));
  }
  const auto offset = this->pptr() - this->pbase();
  this->setp(put_.data() + (other.pbase() - other.put_.data()),
             put_.data() + (other.epptr() - other.put_.data()));
  static_assert(std::tuple_size<decltype(put_)>::value <= unsigned_max<int>(),
                "buffer too large");
  this->pbump(static_cast<int>(offset));
}

file::int_type file::overflow(const int_type ch) {
  assert(this->pptr() == this->epptr());
  if (!this->flush()) return traits_type::eof();
  if (traits_type::eq_int_type(ch, traits_type::eof())) {
    return traits_type::not_eof(0);
  }
  assert(this->pptr() == put_.data());
  assert(this->epptr() > put_.data());
  traits_type::assign(put_.front(), traits_type::to_char_type(ch));
  this->pbump(1);
  return ch;
}

std::streamsize file::xsputn(const char_type* const data,
                             const std::streamsize count) {
  if (!this->flush()) return 0;
  return this->write(data, count).count;
}

file::result file::write(const char* data, std::streamsize count) {
  assert(count >= 0);
  static_assert(unsigned_max<std::streamsize>() <= unsigned_max<std::size_t>(),
                "unsupported architecture");
  result result = {};
  while (count > 0) {
    const auto n = ::write(fd_, data, static_cast<std::size_t>(count));
    if (n < 0) result.error = errno;
    if (n <= 0) break;
    result.count += n;
    data += n;
    count -= n;
  }
  return result;
}

file::int_type file::underflow() {
  assert(this->gptr() == this->egptr());
  static_assert(
      std::tuple_size<decltype(get_)>::value <= unsigned_max<std::streamsize>(),
      "buffer too large");
  const auto result =
      this->read(get_.data(), static_cast<std::streamsize>(get_.size()));
  const auto read = result.count;
  this->setg(get_.data(), get_.data(), get_.data() + read);
  if (result.error != 0 || read == 0) return traits_type::eof();
  assert(this->gptr() != nullptr);
  assert(this->gptr() != this->egptr());
  return traits_type::to_int_type(*this->gptr());
}

std::streamsize file::xsgetn(char_type* data, std::streamsize count) {
  if (count == 0) return 0;
  std::streamsize read = 0;
  static_assert(unsigned_max<std::streamsize>() <= unsigned_max<std::size_t>(),
                "unsupported architecture");
  if (this->gptr() != nullptr && this->egptr() != this->gptr()) {
    read = std::min(count, this->egptr() - this->gptr());
    traits_type::copy(data, this->gptr(), static_cast<std::size_t>(read));
    data += read;
    count -= read;
    this->setg(this->gptr(), this->gptr() + read, this->egptr());
  }
  return read + this->read(data, count).count;
}

file::result file::read(char* data, std::streamsize count) {
  assert(count >= 0);
  static_assert(unsigned_max<std::streamsize>() <= unsigned_max<std::size_t>(),
                "unsupported architecture");
  result result = {};
  while (count > 0) {
    const auto n = ::read(fd_, data, static_cast<std::size_t>(count));
    if (n < 0) result.error = errno;
    if (n <= 0) break;
    result.count += n;
    data += n;
    count -= n;
  }
  return result;
}

int file::sync() {
  const auto success = this->flush();
  if (::fsync(fd_) < 0) {
    throw std::system_error(errno, std::system_category(), "fsync");
  }
  return success ? 0 : -1;
}

[[nodiscard]] bool file::flush() {
  const auto pbase = this->pbase();
  const auto pptr = this->pptr();
  assert(pbase != nullptr);
  assert(pptr != nullptr);
  const auto count = pptr - pbase;
  assert(count >= 0);
  const auto result = this->write(pbase, count);
  const auto written = result.count;
  assert(written >= 0);
  assert(written <= count);
  const auto remaining = count - written;
  if (remaining == 0) {
    this->setp(put_.data(), put_.data() + put_.size());
  } else {
    // If we only managed to do a partial write, we can’t reuse the array.
    // Instead, we set pbase so that the next attempt to flush will start with
    // the yet-unflushed data.
    this->setp(pbase + written, put_.data() + put_.size());
    // Set pptr to its previous value.
    static_assert(std::tuple_size<decltype(put_)>::value <= unsigned_max<int>(),
                  "buffer too large");
    this->pbump(static_cast<int>(remaining));
    assert(this->pptr() == pptr);
  }
  if (result.error != 0) {
    throw std::system_error(result.error, std::system_category(), "write");
  }
  return remaining == 0;
}

temp_file::temp_file(const std::string& directory, const absl::string_view tmpl,
                     random& random) {
  for (int i = 0; i < 10; i++) {
    auto name = join_path(directory, random.temp_name(tmpl));
    if (!file_exists(name)) {
      this->open(std::move(name),
                 file_mode::readwrite | file_mode::create | file_mode::excl);
      return;
    }
  }
  throw std::runtime_error("can’t create temporary file in directory " +
                           directory + " with template " + std::string(tmpl));
}

temp_file::~temp_file() noexcept {
  const auto& path = this->path();
  const auto code = this->remove();
  // Only print an error if removing the file failed (“false” return value), but
  // the file wasn’t already removed before (zero error code).
  if (code && code != std::errc::no_such_file_or_directory) {
    std::clog << "error removing temporary file " << path << ": " << code
              << ": " << code.message() << std::endl;
  }
}

void temp_file::do_close() {
  const auto code = this->remove();
  if (code) throw std::system_error(code);
}

[[nodiscard]] std::error_code temp_file::remove() noexcept {
  return remove_file(this->path());
}

static constexpr absl::string_view remove_prefix(const absl::string_view string,
                                                 const char prefix) noexcept {
  return (string.empty() || string.front() != prefix) ? string
                                                      : string.substr(1);
}

static constexpr absl::string_view remove_suffix(const absl::string_view string,
                                                 const char suffix) noexcept {
  return (string.empty() || string.back() != suffix)
             ? string
             : string.substr(0, string.size() - 1);
}

std::string join_path(const absl::string_view a, const absl::string_view b) {
  return std::string(remove_slash(a)) + '/' +
         std::string(remove_prefix(b, '/'));
}

std::string make_absolute(const absl::string_view name) {
  if (is_absolute(name)) return std::string(name);
  struct free {
    void operator()(void* const ptr) const noexcept { std::free(ptr); }
  };
  const std::unique_ptr<char, free> dir(get_current_dir_name());
  if (dir == nullptr) {
    throw std::system_error(errno, std::system_category(),
                            "get_current_dir_name");
  }
  return join_path(dir.get(), name);
}

[[nodiscard]] bool file_exists(const std::string& name) noexcept {
  struct stat info;
  return ::lstat(name.c_str(), &info) == 0;
}

[[nodiscard]] std::error_code remove_file(const std::string& name) noexcept {
  return std::error_code(::unlink(name.c_str()) == 0 ? 0 : errno,
                         std::system_category());
}

[[nodiscard]] std::string temp_dir() {
  const std::array<const char*, 2> vars = {"TEST_TMPDIR", "TMPDIR"};
  for (const auto var : vars) {
    const auto value = std::getenv(var);
    if (value != nullptr && *value != '\0') return value;
  }
  return "/tmp";
}

directory::directory(const std::string& name) : dir_(::opendir(name.c_str())) {
  if (dir_ == nullptr) {
    throw std::system_error(errno, std::system_category(),
                            "opendir(" + name + ')');
  }
}

directory::~directory() noexcept {
  const auto code = this->close();
  if (code) std::clog << code << ": " << code.message() << std::endl;
}

[[nodiscard]] std::error_code directory::close() noexcept {
  if (dir_ == nullptr) return std::error_code();
  const std::error_code code(::closedir(dir_) == 0 ? 0 : errno,
                             std::system_category());
  dir_ = nullptr;
  return code;
}

void directory::iterator::advance() {
  errno = 0;
  const auto entry = ::readdir(dir_);
  if (entry == nullptr) {
    dir_ = nullptr;
    if (errno != 0) {
      throw std::system_error(errno, std::system_category(), "readdir");
    }
    entry_.clear();
  } else {
    entry_ = entry->d_name;
  }
}

}  // phst_rules_elisp
