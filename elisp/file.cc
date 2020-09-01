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

#include "elisp/file.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <fcntl.h>
#include <unistd.h>

#include <array>
#include <cassert>
#include <cerrno>
#include <cstdint>
#include <cstdlib>
#include <initializer_list>
#include <iostream>
#include <memory>
#include <random>
#include <string>
#include <type_traits>
#include <utility>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#include "absl/base/attributes.h"
#include "absl/meta/type_traits.h"
#include "absl/random/random.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "absl/strings/string_view.h"
#include "absl/strings/strip.h"
#include "absl/utility/utility.h"
#pragma GCC diagnostic pop

#include "elisp/status.h"
#include "elisp/str.h"

namespace phst_rules_elisp {

namespace {

template <typename T>
constexpr absl::make_unsigned_t<T> ToUnsigned(const T n) noexcept {
  static_assert(std::is_integral<T>::value, "type is not integral");
  return static_cast<absl::make_unsigned_t<T>>(n);
}

}  // namespace

absl::StatusOr<TempFile> TempFile::Create(const std::string& directory,
                                          const absl::string_view tmpl,
                                          absl::BitGen& random) {
  for (int i = 0; i < 10; i++) {
    auto name = TempName(directory, tmpl, random);
    if (!FileExists(name)) {
      int fd = -1;
      while (true) {
        fd = ::open(Pointer(name), O_RDWR | O_CREAT | O_EXCL | O_CLOEXEC,
                    S_IRUSR | S_IWUSR);
        if (fd >= 0 || errno != EINTR) break;
      }
      if (fd < 0) return ErrnoStatus("open", name);
      return TempFile(fd, std::move(name));
    }
  }
  return absl::UnavailableError(
      absl::StrCat("can’t create temporary file in directory ", directory,
                   " with template ", tmpl));
}

TempFile::TempFile(TempFile&& other)
    : fd_(absl::exchange(other.fd_, -1)), path_(std::move(other.path_)) {}

TempFile& TempFile::operator=(TempFile&& other) {
  const auto status = this->Close();
  if (!status.ok()) std::clog << status << std::endl;
  fd_ = absl::exchange(other.fd_, -1);
  path_ = std::move(other.path_);
  return *this;
}

TempFile::~TempFile() noexcept {
  const auto status = this->Close();
  // Only print an error if removing the file failed (status not OK), but
  // the file wasn’t already removed before (NOT_FOUND status).
  if (!status.ok() && !absl::IsNotFound(status)) {
    std::clog << "error removing temporary file " << this->path() << ": "
              << status << std::endl;
  }
}

absl::Status TempFile::Close() {
  auto status = RemoveFile(this->path());
  status.Update(this->Flush());
  status.Update(this->CloseHandle());
  return status;
}

absl::Status TempFile::Flush() {
  int status = -1;
  while (true) {
    status = ::fsync(fd_);
    if (status == 0 || errno != EINTR) break;
  }
  if (status != 0) return this->Fail("fsync");
  return absl::OkStatus();
}

absl::Status TempFile::CloseHandle() {
  if (fd_ < 0) return absl::OkStatus();
  // Don’t check for EINTR, see https://lwn.net/Articles/576478/.
  const int status = ::close(fd_);
  path_.clear();
  if (status != 0) return this->Fail("close");
  return absl::OkStatus();
}

absl::Status TempFile::Write(const absl::string_view data) {
  auto rest = data;
  while (!rest.empty()) {
    const auto n = ::write(fd_, rest.data(), rest.size());
    if (n < 0) return this->Fail("write");
    if (n == 0) break;
    rest.remove_prefix(ToUnsigned(n));
  }
  if (!rest.empty()) {
    return absl::DataLossError(
        absl::StrCat("requested write of ", data.size(), " bytes, but only ",
                     data.size() - rest.size(), " bytes written"));
  }
  return this->Flush();
}

absl::Status TempFile::Fail(const absl::string_view function) const {
  const auto status = ErrnoStatus(function);
  return absl::Status(status.code(),
                      absl::StrCat("file ", path_, ": ", status.message()));
}

std::string JoinPathImpl(const std::initializer_list<absl::string_view> pieces) {
  assert(pieces.begin() < pieces.end());
  const auto format = [](std::string* const out, absl::string_view name) {
    absl::ConsumePrefix(&name, "/");
    absl::ConsumeSuffix(&name, "/");
    out->append(name.begin(), name.end());
  };
  // Make sure to not strip leading or trailing slashes.
  const auto first = *pieces.begin();
  const auto last = *(pieces.end() - 1);
  return absl::StrCat((first.length() > 1 && first.front() == '/') ? "/" : "",
                      absl::StrJoin(pieces, "/", format),
                      (last.length() > 1 && last.back() == '/') ? "/" : "");
}

absl::StatusOr<std::string> MakeAbsolute(const absl::string_view name) {
  if (IsAbsolute(name)) return std::string(name);
  struct free {
    void operator()(void* const ptr) const noexcept { std::free(ptr); }
  };
  const std::unique_ptr<char, free> dir(::getcwd(nullptr, 0));
  if (dir == nullptr) return ErrnoStatus("get_current_dir_name");
  return JoinPath(dir.get(), name);
}

absl::StatusOr<std::string> MakeRelative(const absl::string_view name,
                                         std::string root) {
  ASSIGN_OR_RETURN(const auto abs, MakeAbsolute(name));
  assert(!root.empty());
  if (root.back() != '/') root += '/';
  absl::string_view result = abs;
  absl::ConsumePrefix(&result, root);
  return std::string(result);
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

ABSL_MUST_USE_RESULT std::string TempName(const absl::string_view dir,
                                          const absl::string_view tmpl,
                                          absl::BitGen& random) {
  const auto pos = tmpl.rfind('*');
  if (pos == tmpl.npos) {
    std::clog << "no * in template " << tmpl << std::endl;
    std::abort();
  }
  const auto prefix = tmpl.substr(0, pos);
  const auto suffix = tmpl.substr(pos + 1);
  std::uniform_int_distribution<std::uint64_t> distribution;
  const std::string name = absl::StrCat(
      prefix, absl::Hex(distribution(random), absl::kZeroPad16), suffix);
  return JoinPath(dir, name);
}

absl::StatusOr<Directory> Directory::Open(const std::string& name) {
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
  if (::closedir(dir_) != 0) return ErrnoStatus("closedir");
  dir_ = nullptr;
  return absl::OkStatus();
}

absl::StatusOr<std::string> Directory::Read() {
  errno = 0;
  const auto entry = ::readdir(dir_);
  if (entry == nullptr) {
    if (errno != 0) return ErrnoStatus("readdir");
    return std::string();  // end of stream
  }
  return absl::implicit_cast<std::string>(entry->d_name);
}

}  // phst_rules_elisp
