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
#include <initializer_list>
#include <ios>
#include <iostream>
#include <memory>
#include <string>
#include <system_error>
#include <utility>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#include "absl/base/attributes.h"
#include "absl/status/status.h"
#include "absl/strings/match.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "absl/strings/string_view.h"
#include "absl/strings/strip.h"
#include "absl/utility/utility.h"
#pragma GCC diagnostic pop

#include "internal/int.h"
#include "internal/random.h"
#include "internal/str.h"

namespace phst_rules_elisp {

static absl::StatusCode MapErrorCode(const std::error_code& code) {
  const auto condition = code.default_error_condition();
  if (condition.category() != std::generic_category()) {
    return absl::StatusCode::kUnknown;
  }
  switch (static_cast<std::errc>(condition.value())) {
    case std::errc::file_exists:
      return absl::StatusCode::kAlreadyExists;
    case std::errc::function_not_supported:
      return absl::StatusCode::kUnimplemented;
    case std::errc::no_space_on_device:
      return absl::StatusCode::kResourceExhausted;
    case std::errc::no_such_file_or_directory:
      return absl::StatusCode::kNotFound;
    case std::errc::operation_canceled:
      return absl::StatusCode::kCancelled;
    case std::errc::permission_denied:
      return absl::StatusCode::kPermissionDenied;
    case std::errc::timed_out:
      return absl::StatusCode::kDeadlineExceeded;
    default:
      return absl::StatusCode::kUnknown;
  }
}

absl::Status MakeErrorStatus(const std::error_code& code,
                             const absl::string_view function,
                             const absl::string_view args) {
  if (!code) return absl::OkStatus();
  return absl::Status(
      MapErrorCode(code),
      absl::StrCat(function, args.empty() ? args : absl::StrCat("(", args, ")"),
                   ": ", code.category().name(), "/", code.value(), ": ",
                   code.message()));
}

absl::Status StreamStatus(const std::ios& stream) {
  if (stream.good()) return absl::OkStatus();
  if (stream.bad()) return absl::DataLossError("stream is bad");
  if (stream.fail()) return absl::FailedPreconditionError("stream has failed");
  if (stream.eof()) return absl::OutOfRangeError("end of file reached");
  std::abort();  // can’t happen
}

StatusOr<File> File::Open(std::string path, const FileMode mode) {
  File result;
  RETURN_IF_ERROR(result.DoOpen(std::move(path), mode));
  return result;
}

File::File() { this->setp(put_.data(), put_.data() + put_.size()); }

File::File(File&& other)
    : std::streambuf(other),
      fd_(other.fd_),
      get_(other.get_),
      put_(other.put_),
      path_(std::move(other.path_)),
      status_(std::move(other.status_)) {
  this->Move(std::move(other));
}

File& File::operator=(File&& other) {
  const auto status = this->Close();
  if (!status.ok()) std::clog << status << std::endl;
  this->std::streambuf::operator=(other);
  fd_ = other.fd_;
  get_ = other.get_;
  put_ = other.put_;
  path_ = std::move(other.path_);
  status_ = std::move(other.status_);
  this->Move(std::move(other));
  return *this;
}

File::~File() noexcept {
  if (fd_ < 0) return;
  std::clog << "file " << path_ << " still open" << std::endl;
  const auto status = this->Close();
  if (!status.ok()) std::clog << "error closing file: " << status << std::endl;
}

absl::Status File::DoOpen(std::string path, const FileMode mode) {
  int fd = -1;
  while (true) {
    fd = ::open(Pointer(path), static_cast<int>(mode) | O_CLOEXEC,
                S_IRUSR | S_IWUSR);
    if (fd >= 0 || errno != EINTR) break;
  }
  if (fd < 0) return ErrnoStatus("open", path);
  fd_ = fd;
  path_ = std::move(path);
  return status_;
}

StatusOr<std::FILE*> File::OpenCFile(const char* const mode) {
  const auto file = fdopen(fd_, mode);
  if (file == nullptr) return ErrnoStatus("fdopen");
  return file;
}

absl::Status File::Close() {
  if (fd_ < 0) return status_;
  RETURN_IF_ERROR(this->Flush());
  int status = -1;
  while (true) {
    status = ::close(fd_);
    if (status == 0 || errno != EINTR) break;
  }
  fd_ = -1;
  if (status != 0) return this->Fail("close");
  status_.Update(this->DoClose());
  path_.clear();
  return status_;
}

absl::Status File::DoClose() { return absl::OkStatus(); }

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
  if (!this->Flush().ok()) return traits_type::eof();
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
  if (!this->Flush().ok()) return 0;
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
  this->Flush().IgnoreError();
  if (::fsync(fd_) != 0) {
    this->Fail("fdsync").IgnoreError();
    return -1;
  }
  return status_.ok() ? 0 : -1;
}

absl::Status File::Flush() {
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
    return ErrorStatus(std::error_code(result.error, std::system_category()),
                       "write");
  }
  return remaining == 0 ? absl::OkStatus()
                        : absl::DataLossError(
                              absl::StrCat(remaining, " bytes not written"));
}

const absl::Status& File::Fail(absl::Status status) {
  status_.Update(std::move(status));
  return status_;
}

const absl::Status& File::Fail(const absl::string_view function) {
  const auto status = ErrnoStatus(function);
  return this->Fail(absl::Status(
      status.code(), absl::StrCat("file ", path_, ": ", status.message())));
}

StatusOr<TempFile> TempFile::Open(const std::string& directory,
                                  const absl::string_view tmpl,
                                  Random& random) {
  TempFile result;
  for (int i = 0; i < 10; i++) {
    auto name = JoinPath(directory, random.TempName(tmpl));
    if (!FileExists(name)) {
      RETURN_IF_ERROR(result.DoOpen(std::move(name), FileMode::kReadWrite |
                                                         FileMode::kCreate |
                                                         FileMode::kExclusive));
      return result;
    }
  }
  return absl::UnavailableError(
      absl::StrCat("can’t create temporary file in directory ", directory,
                   " with template ", tmpl));
}

TempFile::TempFile() {}

TempFile::~TempFile() noexcept {
  const auto& path = this->path();
  const auto status = this->Remove();
  // Only print an error if removing the file failed (“false” return value), but
  // the file wasn’t already removed before (zero error code).
  if (!status.ok() && !absl::IsNotFound(status)) {
    std::clog << "error removing temporary file " << path << ": " << status
              << std::endl;
  }
}

absl::Status TempFile::DoClose() {
  return this->Remove();
}

absl::Status TempFile::Remove() noexcept {
  return RemoveFile(this->path());
}

std::string JoinPathImpl(const std::initializer_list<absl::string_view> pieces) {
  assert(pieces.begin() < pieces.end());
  const auto format = [](std::string* const out, absl::string_view name) {
    absl::ConsumePrefix(&name, "/");
    absl::ConsumeSuffix(&name, "/");
    out->append(name.begin(), name.end());
  };
  // Make sure to not strip leading or trailing slashes.
  return absl::StrCat(absl::StartsWith(*pieces.begin(), "/") ? "/" : "",
                      absl::StrJoin(pieces, "/", format),
                      absl::EndsWith(*(pieces.end() - 1), "/") ? "/" : "");
}

StatusOr<std::string> MakeAbsolute(const absl::string_view name) {
  if (IsAbsolute(name)) return std::string(name);
  struct free {
    void operator()(void* const ptr) const noexcept { std::free(ptr); }
  };
  const std::unique_ptr<char, free> dir(get_current_dir_name());
  if (dir == nullptr) return ErrnoStatus("get_current_dir_name");
  return JoinPath(dir.get(), name);
}

ABSL_MUST_USE_RESULT bool FileExists(const std::string& name) noexcept {
  struct stat info;
  return ::lstat(Pointer(name), &info) == 0;
}

absl::Status RemoveFile(const std::string& name) noexcept {
  return ::unlink(Pointer(name)) == 0 ? absl::OkStatus()
                                      : ErrnoStatus("unlink", name);
}

ABSL_MUST_USE_RESULT std::string TempDir() {
  const std::array<const char*, 2> vars = {"TEST_TMPDIR", "TMPDIR"};
  for (const auto var : vars) {
    const auto value = std::getenv(var);
    if (value != nullptr && *value != '\0') return value;
  }
  return "/tmp";
}

StatusOr<Directory> Directory::Open(const std::string& name) {
  const auto dir = ::opendir(Pointer(name));
  if (dir == nullptr) return ErrnoStatus("opendir", name);
  return Directory(dir);
}

Directory& Directory::operator=(Directory&& other) {
  const auto status = this->Close();
  if (!status.ok()) std::clog << status << std::endl;
  dir_ = absl::exchange(other.dir_, nullptr);
  return *this;
}

Directory::~Directory() noexcept {
  const auto status = this->Close();
  if (!status.ok()) std::clog << status << std::endl;
}

absl::Status Directory::Close() noexcept {
  if (dir_ == nullptr) return absl::OkStatus();
  const auto status =
      ::closedir(dir_) == 0 ? absl::OkStatus() : ErrnoStatus("closedir");
  dir_ = nullptr;
  return status;
}

void Directory::Iterator::Advance() {
  errno = 0;
  const auto entry = ::readdir(dir_);
  if (entry == nullptr) {
    dir_ = nullptr;
    if (errno != 0) std::clog << ErrnoStatus("readdir") << std::endl;
    entry_.clear();
  } else {
    entry_ = entry->d_name;
  }
}

}  // phst_rules_elisp
