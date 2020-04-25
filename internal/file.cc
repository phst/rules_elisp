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

#include "absl/base/attributes.h"
#include "absl/strings/string_view.h"
#include "absl/utility/utility.h"

#include "internal/int.h"
#include "internal/random.h"

namespace phst_rules_elisp {

File::File() { this->setp(put_.data(), put_.data() + put_.size()); }

File::File(std::string path, const FileMode mode) : File() {
  this->Open(std::move(path), mode);
}

File::File(File&& other)
    : std::streambuf(other),
      fd_(other.fd_),
      get_(other.get_),
      put_(other.put_),
      path_(std::move(other.path_)) {
  this->Move(std::move(other));
}

File& File::operator=(File&& other) {
  this->std::streambuf::operator=(other);
  fd_ = other.fd_;
  get_ = other.get_;
  put_ = other.put_;
  path_ = std::move(other.path_);
  this->Move(std::move(other));
  return *this;
}

File::~File() noexcept {
  if (fd_ >= 0) std::clog << "file " << path_ << " still open" << std::endl;
}

void File::Open(std::string path, const FileMode mode) {
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

std::FILE* File::OpenCFile(const char* const mode) {
  const auto file = fdopen(fd_, mode);
  if (file == nullptr) {
    throw std::system_error(errno, std::system_category(), "fdopen");
  }
  return file;
}

void File::Close() {
  if (fd_ < 0) return;
  if (!this->Flush()) {
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
  this->DoClose();
  path_.clear();
}

void File::DoClose() {}

void File::Move(File&& other) {
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
  static_assert(std::tuple_size<decltype(put_)>::value <= UnsignedMax<int>(),
                "buffer too large");
  this->pbump(static_cast<int>(offset));
}

File::int_type File::overflow(const int_type ch) {
  assert(this->pptr() == this->epptr());
  if (!this->Flush()) return traits_type::eof();
  if (traits_type::eq_int_type(ch, traits_type::eof())) {
    return traits_type::not_eof(0);
  }
  assert(this->pptr() == put_.data());
  assert(this->epptr() > put_.data());
  traits_type::assign(put_.front(), traits_type::to_char_type(ch));
  this->pbump(1);
  return ch;
}

std::streamsize File::xsputn(const char_type* const data,
                             const std::streamsize count) {
  if (!this->Flush()) return 0;
  return this->Write(data, count).count;
}

File::Result File::Write(const char* data, std::streamsize count) {
  assert(count >= 0);
  static_assert(UnsignedMax<std::streamsize>() <= UnsignedMax<std::size_t>(),
                "unsupported architecture");
  Result result = {};
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

File::int_type File::underflow() {
  assert(this->gptr() == this->egptr());
  static_assert(
      std::tuple_size<decltype(get_)>::value <= UnsignedMax<std::streamsize>(),
      "buffer too large");
  const auto result =
      this->Read(get_.data(), static_cast<std::streamsize>(get_.size()));
  const auto read = result.count;
  this->setg(get_.data(), get_.data(), get_.data() + read);
  if (result.error != 0 || read == 0) return traits_type::eof();
  assert(this->gptr() != nullptr);
  assert(this->gptr() != this->egptr());
  return traits_type::to_int_type(*this->gptr());
}

std::streamsize File::xsgetn(char_type* data, std::streamsize count) {
  if (count == 0) return 0;
  std::streamsize read = 0;
  static_assert(UnsignedMax<std::streamsize>() <= UnsignedMax<std::size_t>(),
                "unsupported architecture");
  if (this->gptr() != nullptr && this->egptr() != this->gptr()) {
    read = std::min(count, this->egptr() - this->gptr());
    traits_type::copy(data, this->gptr(), static_cast<std::size_t>(read));
    data += read;
    count -= read;
    this->setg(this->gptr(), this->gptr() + read, this->egptr());
  }
  return read + this->Read(data, count).count;
}

File::Result File::Read(char* data, std::streamsize count) {
  assert(count >= 0);
  static_assert(UnsignedMax<std::streamsize>() <= UnsignedMax<std::size_t>(),
                "unsupported architecture");
  Result result = {};
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

int File::sync() {
  const auto success = this->Flush();
  if (::fsync(fd_) < 0) {
    throw std::system_error(errno, std::system_category(), "fsync");
  }
  return success ? 0 : -1;
}

ABSL_MUST_USE_RESULT bool File::Flush() {
  const auto pbase = this->pbase();
  const auto pptr = this->pptr();
  assert(pbase != nullptr);
  assert(pptr != nullptr);
  const auto count = pptr - pbase;
  assert(count >= 0);
  const auto result = this->Write(pbase, count);
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
    static_assert(std::tuple_size<decltype(put_)>::value <= UnsignedMax<int>(),
                  "buffer too large");
    this->pbump(static_cast<int>(remaining));
    assert(this->pptr() == pptr);
  }
  if (result.error != 0) {
    throw std::system_error(result.error, std::system_category(), "write");
  }
  return remaining == 0;
}

TempFile::TempFile(const std::string& directory, const absl::string_view tmpl,
                   Random& random) {
  for (int i = 0; i < 10; i++) {
    auto name = JoinPath(directory, random.TempName(tmpl));
    if (!FileExists(name)) {
      this->Open(std::move(name), FileMode::kReadWrite | FileMode::kCreate |
                                      FileMode::kExclusive);
      return;
    }
  }
  throw std::runtime_error("can’t create temporary file in directory " +
                           directory + " with template " + std::string(tmpl));
}

TempFile::~TempFile() noexcept {
  const auto& path = this->path();
  const auto code = this->Remove();
  // Only print an error if removing the file failed (“false” return value), but
  // the file wasn’t already removed before (zero error code).
  if (code && code != std::errc::no_such_file_or_directory) {
    std::clog << "error removing temporary file " << path << ": " << code
              << ": " << code.message() << std::endl;
  }
}

void TempFile::DoClose() {
  const auto code = this->Remove();
  if (code) throw std::system_error(code);
}

ABSL_MUST_USE_RESULT std::error_code TempFile::Remove() noexcept {
  return RemoveFile(this->path());
}

static constexpr absl::string_view RemovePrefix(const absl::string_view string,
                                                const char prefix) noexcept {
  return (string.empty() || string.front() != prefix) ? string
                                                      : string.substr(1);
}

static constexpr absl::string_view RemoveSuffix(const absl::string_view string,
                                                const char suffix) noexcept {
  return (string.empty() || string.back() != suffix)
             ? string
             : string.substr(0, string.size() - 1);
}

std::string JoinPath(const absl::string_view a, const absl::string_view b) {
  return std::string(RemoveSlash(a)) + '/' + std::string(RemovePrefix(b, '/'));
}

std::string MakeAbsolute(const absl::string_view name) {
  if (IsAbsolute(name)) return std::string(name);
  struct free {
    void operator()(void* const ptr) const noexcept { std::free(ptr); }
  };
  const std::unique_ptr<char, free> dir(get_current_dir_name());
  if (dir == nullptr) {
    throw std::system_error(errno, std::system_category(),
                            "get_current_dir_name");
  }
  return JoinPath(dir.get(), name);
}

ABSL_MUST_USE_RESULT bool FileExists(const std::string& name) noexcept {
  struct stat info;
  return ::lstat(name.c_str(), &info) == 0;
}

ABSL_MUST_USE_RESULT std::error_code RemoveFile(
    const std::string& name) noexcept {
  return std::error_code(::unlink(name.c_str()) == 0 ? 0 : errno,
                         std::system_category());
}

ABSL_MUST_USE_RESULT std::string TempDir() {
  const std::array<const char*, 2> vars = {"TEST_TMPDIR", "TMPDIR"};
  for (const auto var : vars) {
    const auto value = std::getenv(var);
    if (value != nullptr && *value != '\0') return value;
  }
  return "/tmp";
}

Directory::Directory(const std::string& name) : dir_(::opendir(name.c_str())) {
  if (dir_ == nullptr) {
    throw std::system_error(errno, std::system_category(),
                            "opendir(" + name + ')');
  }
}

Directory& Directory::operator=(Directory&& other) {
  const auto code = this->Close();
  if (code) std::clog << code << ": " << code.message() << std::endl;
  dir_ = absl::exchange(other.dir_, nullptr);
  return *this;
}

Directory::~Directory() noexcept {
  const auto code = this->Close();
  if (code) std::clog << code << ": " << code.message() << std::endl;
}

ABSL_MUST_USE_RESULT std::error_code Directory::Close() noexcept {
  if (dir_ == nullptr) return std::error_code();
  const std::error_code code(::closedir(dir_) == 0 ? 0 : errno,
                             std::system_category());
  dir_ = nullptr;
  return code;
}

void Directory::Iterator::Advance() {
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
