// Copyright 2025 Philipp Stephani
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include "elisp/private/tools/system.h"

#include <cerrno>
#include <cstdlib>
#include <fstream>
#include <ios>
#include <iterator>
#include <locale>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <vector>

#include "absl/base/nullability.h"
#include "absl/log/check.h"
#include "absl/status/status.h"
#include "absl/status/status_matchers.h"
#include "absl/status/statusor.h"
#include "absl/strings/ascii.h"
#include "absl/strings/str_format.h"
#include "absl/strings/str_cat.h"
#include "absl/time/clock.h"
#include "absl/time/time.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

#include "elisp/private/tools/numeric.h"
#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/runfiles.h"
#include "elisp/private/tools/strings.h"

namespace rules_elisp {

using absl_testing::IsOk;
using absl_testing::IsOkAndHolds;
using absl_testing::StatusIs;
using ::testing::_;
using ::testing::AnyOf;
using ::testing::Contains;
using ::testing::ElementsAre;
using ::testing::EndsWith;
using ::testing::Ge;
using ::testing::Gt;
using ::testing::HasSubstr;
using ::testing::IsEmpty;
using ::testing::Not;
using ::testing::Pair;
using ::testing::SizeIs;
using ::testing::StartsWith;
using ::testing::TestWithParam;
using ::testing::Values;

static absl::StatusOr<std::string> ReadFile(const NativeStringView name) {
  std::ifstream stream(NativeString(name), std::ios::in | std::ios::binary);
  if (!stream.is_open() || !stream.good()) {
    return absl::UnknownError(
        absl::StrFormat("Cannot open file %s for reading", name));
  }
  stream.imbue(std::locale::classic());

  std::ostringstream buffer;
  buffer.imbue(std::locale::classic());
  buffer << stream.rdbuf();
  buffer.flush();
  if (!buffer.good() || !stream.good()) {
    return absl::UnknownError(absl::StrFormat("Cannot read file %s", name));
  }
  return buffer.str();
}

static absl::Status WriteFile(const NativeStringView name,
                              const std::string_view contents) {
  std::ofstream stream(NativeString(name),
                       std::ios::out | std::ios::trunc | std::ios::binary);
  if (!stream.is_open() || !stream.good()) {
    return absl::UnknownError(
        absl::StrFormat("Cannot open file %s for writing", name));
  }
  stream.imbue(std::locale::classic());
  const std::optional<std::streamsize> count =
      CastNumber<std::streamsize>(contents.size());
  if (!count.has_value()) {
    return absl::InvalidArgumentError(
        absl::StrFormat("Content too big (%d bytes)", contents.size()));
  }
  stream.write(contents.data(), *count);
  stream.flush();
  if (!stream.good()) {
    return absl::DataLossError(
        absl::StrFormat("Cannot write %d bytes to file %s", *count, name));
  }
  return absl::OkStatus();
}

TEST(ErrorStatusTest, FormatsArguments) {
  EXPECT_THAT(
      ErrorStatus(std::make_error_code(std::errc::interrupted), "fóo", "bár",
                  L"báz", 42, absl::Hex(0x42), Oct(042), nullptr),
      StatusIs(_, HasSubstr("fóo(\"bár\", L\"báz\", 42, 0x42, 0042, nullptr)")));
}

TEST(ErrnoStatusTest, ReturnsMatchingStatus) {
  errno = ENOENT;
  EXPECT_THAT(ErrnoStatus("foo"), StatusIs(absl::StatusCode::kNotFound));
}

TEST(ToNarrowTest, AcceptsAscii) {
  EXPECT_THAT(ToNarrow(RULES_ELISP_NATIVE_LITERAL(""), Encoding::kAscii),
              IsOkAndHolds(""));
  EXPECT_THAT(ToNarrow(RULES_ELISP_NATIVE_LITERAL("Foo"), Encoding::kAscii),
              IsOkAndHolds("Foo"));
}

TEST(ToNarrowTest, RejectsNonAscii) {
  if constexpr (kWindows) {
    EXPECT_THAT(ToNarrow(RULES_ELISP_NATIVE_LITERAL("Foó"), Encoding::kAscii),
                StatusIs(absl::StatusCode::kInvalidArgument));
  }
}

TEST(ToNarrowTest, AcceptsUtf8) {
  EXPECT_THAT(ToNarrow(RULES_ELISP_NATIVE_LITERAL("Foó"), Encoding::kUtf8),
              IsOkAndHolds("Foó"));
}

TEST(ToNarrowTest, RejectsInvalidUtf16OnWindows) {
#ifdef _WIN32
  EXPECT_THAT(ToNarrow(std::wstring{L'F', L'o', L'ó', 0xD800}, Encoding::kUtf8),
              Not(IsOk()));
#endif
}

TEST(ToNativeTest, AcceptsAscii) {
  EXPECT_THAT(ToNative("", Encoding::kAscii),
              IsOkAndHolds(RULES_ELISP_NATIVE_LITERAL("")));
  EXPECT_THAT(ToNative("Foo", Encoding::kAscii),
              IsOkAndHolds(RULES_ELISP_NATIVE_LITERAL("Foo")));
}

TEST(ToNativeTest, RejectsNonAscii) {
  if constexpr (kWindows) {
    EXPECT_THAT(ToNative("Foó", Encoding::kAscii),
                StatusIs(absl::StatusCode::kInvalidArgument));
  }
}

TEST(ToNativeTest, AcceptsUtf8) {
  EXPECT_THAT(ToNative("Foó", Encoding::kUtf8),
              IsOkAndHolds(RULES_ELISP_NATIVE_LITERAL("Foó")));
}

TEST(ToNativeTest, RejectsInvalidUtf8OnWindows) {
  if constexpr (kWindows) {
    EXPECT_THAT(ToNative("Foó\xFF", Encoding::kUtf8), Not(IsOk()));
  } else {
    EXPECT_THAT(ToNative("Foó\xFF", Encoding::kUtf8),
                IsOkAndHolds(RULES_ELISP_NATIVE_LITERAL("Foó\xFF")));
  }
}

struct IsAbsoluteParam final {
  NativeStringView name;
  bool absolute;

  template <typename Sink>
  friend void AbslStringify(Sink& sink, const IsAbsoluteParam& param) {
    absl::Format(sink, "Filename %s should be %s", param.name,
                 param.absolute ? "absolute" : "relative");
  }
};

class IsAbsoluteTest : public TestWithParam<IsAbsoluteParam> {};

TEST_P(IsAbsoluteTest, Works) {
  const IsAbsoluteParam& param = this->GetParam();
  EXPECT_EQ(IsAbsolute(param.name), param.absolute);
}

INSTANTIATE_TEST_SUITE_P(
    VariousFilenames, IsAbsoluteTest,
    Values(
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL(""), false},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("."), false},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("foo"), false},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("foo/bar"), false},
        // See
        // https://googleprojectzero.blogspot.com/2016/02/the-definitive-guide-on-win32-to-nt.html
        // for the various kinds of filenames on Windows.
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("C:\\"), kWindows},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("C:/"), kWindows},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("C:\\Foo"), kWindows},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("C:/Foo"), kWindows},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("C:"), false},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("C:Foo"), false},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("\\Foo"), false},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("/Foo"), !kWindows},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("NUL"), false},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("/"), !kWindows},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("/foo"), !kWindows}));

TEST(MakeAbsoluteTest, RejectsEmptyName) {
  EXPECT_THAT(MakeAbsolute(RULES_ELISP_NATIVE_LITERAL("")),
              StatusIs(absl::StatusCode::kInvalidArgument));
}

TEST(MakeAbsoluteTest, KeepsAbsoluteName) {
  constexpr NativeStringView file =
      kWindows ? RULES_ELISP_NATIVE_LITERAL("C:\\Foo\\Bar.txt")
               : RULES_ELISP_NATIVE_LITERAL("/foo/bar.txt");
  EXPECT_THAT(MakeAbsolute(file), IsOkAndHolds(file));
}

TEST(MakeAbsoluteTest, MakesRelativeNameAbsolute) {
  const absl::StatusOr<NativeString> file =
      MakeAbsolute(RULES_ELISP_NATIVE_LITERAL("foo.txt"));
  ASSERT_THAT(file, IsOk());
  if constexpr (kWindows) {
    ASSERT_THAT(*file, SizeIs(Gt(3)));
    const std::optional<unsigned char> drive =
        CastNumber<unsigned char>(file->at(0));
    ASSERT_NE(drive, std::nullopt);
    EXPECT_TRUE(absl::ascii_isalpha(*drive));
    EXPECT_EQ(file->at(1), RULES_ELISP_NATIVE_LITERAL(':'));
    EXPECT_EQ(file->at(2), RULES_ELISP_NATIVE_LITERAL('\\'));
    EXPECT_THAT(*file, EndsWith(RULES_ELISP_NATIVE_LITERAL("\\foo.txt")));
  } else {
    EXPECT_THAT(*file, StartsWith(RULES_ELISP_NATIVE_LITERAL("/")));
    EXPECT_THAT(*file, EndsWith(RULES_ELISP_NATIVE_LITERAL("/foo.txt")));
  }
}

TEST(MakeRelativeTest, RejectsEmptyName) {
  EXPECT_THAT(MakeRelative(RULES_ELISP_NATIVE_LITERAL(""),
                           RULES_ELISP_NATIVE_LITERAL("")),
              StatusIs(absl::StatusCode::kInvalidArgument));
  EXPECT_THAT(MakeRelative(RULES_ELISP_NATIVE_LITERAL("foo"),
                           RULES_ELISP_NATIVE_LITERAL("")),
              StatusIs(absl::StatusCode::kInvalidArgument));
  EXPECT_THAT(MakeRelative(RULES_ELISP_NATIVE_LITERAL(""),
                           RULES_ELISP_NATIVE_LITERAL("bar")),
              StatusIs(absl::StatusCode::kInvalidArgument));
}

TEST(MakeRelativeTest, RejectsNotWithin) {
  EXPECT_THAT(MakeRelative(RULES_ELISP_NATIVE_LITERAL("foo"),
                           RULES_ELISP_NATIVE_LITERAL("foo")),
              StatusIs(absl::StatusCode::kInvalidArgument));
  EXPECT_THAT(MakeRelative(RULES_ELISP_NATIVE_LITERAL("foo"),
                           RULES_ELISP_NATIVE_LITERAL("bar")),
              StatusIs(absl::StatusCode::kInvalidArgument));
  EXPECT_THAT(MakeRelative(RULES_ELISP_NATIVE_LITERAL("fooooo"),
                           RULES_ELISP_NATIVE_LITERAL("foo")),
              StatusIs(absl::StatusCode::kInvalidArgument));
  if constexpr (!kWindows) {
    EXPECT_THAT(MakeRelative(RULES_ELISP_NATIVE_LITERAL("C:\\Foo\\Bar"),
                             RULES_ELISP_NATIVE_LITERAL("C:\\Foo")),
                StatusIs(absl::StatusCode::kInvalidArgument));
    EXPECT_THAT(MakeRelative(RULES_ELISP_NATIVE_LITERAL("C:\\Foo\\Bar"),
                             RULES_ELISP_NATIVE_LITERAL("C:\\Foo")),
                StatusIs(absl::StatusCode::kInvalidArgument));
  }
}

TEST(MakeRelativeTest, Relativizes) {
  EXPECT_THAT(MakeRelative(RULES_ELISP_NATIVE_LITERAL("foo/bar"),
                           RULES_ELISP_NATIVE_LITERAL("foo")),
              IsOkAndHolds(RULES_ELISP_NATIVE_LITERAL("bar")));
  EXPECT_THAT(MakeRelative(RULES_ELISP_NATIVE_LITERAL("foo/bar"),
                           RULES_ELISP_NATIVE_LITERAL("foo/")),
              IsOkAndHolds(RULES_ELISP_NATIVE_LITERAL("bar")));
  if constexpr (kWindows) {
    EXPECT_THAT(MakeRelative(RULES_ELISP_NATIVE_LITERAL("C:\\Foo\\Bar"),
                             RULES_ELISP_NATIVE_LITERAL("C:\\Foo")),
                IsOkAndHolds(RULES_ELISP_NATIVE_LITERAL("Bar")));
    EXPECT_THAT(MakeRelative(RULES_ELISP_NATIVE_LITERAL("C:\\Foo\\Bar"),
                             RULES_ELISP_NATIVE_LITERAL("C:\\Foo")),
                IsOkAndHolds(RULES_ELISP_NATIVE_LITERAL("Bar")));
  }
}

TEST(FileExistsTest, TestsThatFileExists) {
  const absl::StatusOr<NativeString> dir =
      ToNative(::testing::TempDir(), Encoding::kAscii);
  ASSERT_THAT(dir, IsOkAndHolds(Not(IsEmpty())));
  EXPECT_TRUE(FileExists(*dir));
  EXPECT_FALSE(FileExists(*dir + RULES_ELISP_NATIVE_LITERAL("/nonexistent")));
  EXPECT_FALSE(FileExists(RULES_ELISP_NATIVE_LITERAL("")));
}

TEST(IsNonEmptyDirectoryTest, ReturnsFalseForFile) {
  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(ExecutableKind::kTest, BAZEL_CURRENT_REPOSITORY, {});
  ASSERT_THAT(runfiles, IsOk());
  const absl::StatusOr<NativeString> file = runfiles->Resolve(RULES_ELISP_DATA);
  ASSERT_THAT(file, IsOk());

  EXPECT_FALSE(IsNonEmptyDirectory(*file));
}

TEST(IsNonEmptyDirectoryTest, ReturnsTrueForEmptyDirectory) {
  const absl::StatusOr<NativeString> temp =
      ToNative(::testing::TempDir(), Encoding::kAscii);
  ASSERT_THAT(temp, IsOkAndHolds(Not(IsEmpty())));

  const NativeString dir =
      *temp + kSeparator + RULES_ELISP_NATIVE_LITERAL("nonempty-dir");
  EXPECT_THAT(CreateDirectory(dir), IsOk());

  EXPECT_TRUE(IsNonEmptyDirectory(*temp));
  EXPECT_FALSE(IsNonEmptyDirectory(dir));
}

TEST(IsNonEmptyDirectoryTest, ReturnsFalseForNonEmptyDirectory) {
  const absl::StatusOr<NativeString> temp =
      ToNative(::testing::TempDir(), Encoding::kAscii);
  ASSERT_THAT(temp, IsOkAndHolds(Not(IsEmpty())));

  const NativeString dir =
      *temp + kSeparator + RULES_ELISP_NATIVE_LITERAL("empty-dir");
  EXPECT_THAT(CreateDirectory(dir), IsOk());

  EXPECT_TRUE(IsNonEmptyDirectory(*temp));
  EXPECT_FALSE(IsNonEmptyDirectory(dir));

  const NativeString file =
      dir + kSeparator + RULES_ELISP_NATIVE_LITERAL("file.txt");
  std::ofstream stream(file,
                       std::ios::out | std::ios::trunc | std::ios::binary);
  EXPECT_TRUE(stream.is_open());
  EXPECT_TRUE(stream.good());
  stream.close();

  EXPECT_TRUE(IsNonEmptyDirectory(dir));
}

TEST(UnlinkTest, RemovesFile) {
  const absl::StatusOr<NativeString> temp =
      ToNative(::testing::TempDir(), Encoding::kAscii);
  ASSERT_THAT(temp, IsOkAndHolds(Not(IsEmpty())));

  const NativeString file =
      *temp + kSeparator + RULES_ELISP_NATIVE_LITERAL("unlink-test");

  EXPECT_FALSE(FileExists(file));
  EXPECT_THAT(Unlink(file), StatusIs(absl::StatusCode::kNotFound));

  std::ofstream stream(file,
                       std::ios::out | std::ios::trunc | std::ios::binary);
  EXPECT_TRUE(stream.is_open());
  EXPECT_TRUE(stream.good());
  EXPECT_TRUE(stream.flush());
  EXPECT_TRUE(stream.good());
  stream.close();

  EXPECT_TRUE(FileExists(file));
  EXPECT_THAT(Unlink(file), IsOk());
  EXPECT_FALSE(FileExists(file));
  EXPECT_THAT(Unlink(file), StatusIs(absl::StatusCode::kNotFound));
  EXPECT_FALSE(FileExists(file));
}

TEST(CreateRemoveDirectoryTest, CreatesAndRemovesDirectories) {
  const absl::StatusOr<NativeString> temp =
      ToNative(::testing::TempDir(), Encoding::kAscii);
  ASSERT_THAT(temp, IsOkAndHolds(Not(IsEmpty())));

  const NativeString dir =
      *temp + kSeparator + RULES_ELISP_NATIVE_LITERAL("dir");
  EXPECT_THAT(CreateDirectory(dir), IsOk());

  const NativeString file =
      dir + kSeparator + RULES_ELISP_NATIVE_LITERAL("file");
  std::ofstream stream(file,
                       std::ios::out | std::ios::trunc | std::ios::binary);
  EXPECT_TRUE(stream.is_open());
  EXPECT_TRUE(stream.good());
  EXPECT_TRUE(stream.flush());
  EXPECT_TRUE(stream.good());
  stream.close();

  EXPECT_THAT(RemoveDirectory(dir),
              StatusIs(absl::StatusCode::kFailedPrecondition));
  EXPECT_THAT(Unlink(file), IsOk());
  EXPECT_THAT(RemoveDirectory(dir), IsOk());
}

TEST(ListDirectoryTests, ListsDirectory) {
    const absl::StatusOr<NativeString> temp =
      ToNative(::testing::TempDir(), Encoding::kAscii);
  ASSERT_THAT(temp, IsOkAndHolds(Not(IsEmpty())));

  const NativeString dir =
      *temp + kSeparator + RULES_ELISP_NATIVE_LITERAL("dir äα𝐴🐈'");
  EXPECT_THAT(ListDirectory(dir), StatusIs(absl::StatusCode::kNotFound));
  EXPECT_THAT(CreateDirectory(dir), IsOk());
  EXPECT_THAT(ListDirectory(dir), IsOkAndHolds(IsEmpty()));

  const NativeString file =
      dir + kSeparator + RULES_ELISP_NATIVE_LITERAL("file äα𝐴🐈'");
  std::ofstream stream(file,
                       std::ios::out | std::ios::trunc | std::ios::binary);
  EXPECT_TRUE(stream.is_open());
  EXPECT_TRUE(stream.good());
  EXPECT_TRUE(stream.flush());
  EXPECT_TRUE(stream.good());
  stream.close();

  EXPECT_THAT(
      ListDirectory(dir),
      IsOkAndHolds(ElementsAre(RULES_ELISP_NATIVE_LITERAL("file äα𝐴🐈'"))));

  EXPECT_THAT(Unlink(file), IsOk());
  EXPECT_THAT(ListDirectory(dir), IsOkAndHolds(IsEmpty()));
  EXPECT_THAT(RemoveDirectory(dir), IsOk());
  EXPECT_THAT(ListDirectory(dir), StatusIs(absl::StatusCode::kNotFound));
}

TEST(CopyTreeTest, RejectsSelfCopy) {
  const absl::StatusOr<NativeString> temp =
      ToNative(::testing::TempDir(), Encoding::kAscii);
  ASSERT_THAT(temp, IsOkAndHolds(Not(IsEmpty())));

  EXPECT_THAT(CopyTree(*temp, *temp),
              StatusIs(AnyOf(absl::StatusCode::kAlreadyExists,
                             absl::StatusCode::kNotFound)));
}

TEST(CopyTreeTest, RejectsCopyFromRootDirectory) {
  const absl::StatusOr<NativeString> temp =
      ToNative(::testing::TempDir(), Encoding::kAscii);
  ASSERT_THAT(temp, IsOkAndHolds(Not(IsEmpty())));

  const NativeString to = *temp + kSeparator + RULES_ELISP_NATIVE_LITERAL("to");

  EXPECT_THAT(CopyTree(kWindows ? RULES_ELISP_NATIVE_LITERAL("C:\\")
                                : RULES_ELISP_NATIVE_LITERAL("/"),
                       to),
              StatusIs(absl::StatusCode::kInvalidArgument));

  EXPECT_THAT(RemoveDirectory(to), StatusIs(absl::StatusCode::kNotFound));
}

TEST(CopyTreeTest, RejectsCopyToRootDirectory) {
  const absl::StatusOr<NativeString> temp =
      ToNative(::testing::TempDir(), Encoding::kAscii);
  ASSERT_THAT(temp, IsOkAndHolds(Not(IsEmpty())));

  EXPECT_THAT(CopyTree(*temp, kWindows ? RULES_ELISP_NATIVE_LITERAL("C:\\")
                                       : RULES_ELISP_NATIVE_LITERAL("/")),
              StatusIs(absl::StatusCode::kAlreadyExists));
}

TEST(CopyTreeTest, RejectsCopyToExistingDirectory) {
  const absl::StatusOr<NativeString> temp =
      ToNative(::testing::TempDir(), Encoding::kAscii);
  ASSERT_THAT(temp, IsOkAndHolds(Not(IsEmpty())));

  const NativeString from =
      *temp + kSeparator + RULES_ELISP_NATIVE_LITERAL("from");
  const NativeString to = *temp + kSeparator + RULES_ELISP_NATIVE_LITERAL("to");
  EXPECT_THAT(CreateDirectory(from), IsOk());
  EXPECT_THAT(CreateDirectory(to), IsOk());
  EXPECT_THAT(WriteFile(from + kSeparator + RULES_ELISP_NATIVE_LITERAL("file"),
                        "contents"),
              IsOk());

  EXPECT_THAT(CopyTree(from, to), StatusIs(absl::StatusCode::kAlreadyExists));

  EXPECT_THAT(Unlink(from + kSeparator + RULES_ELISP_NATIVE_LITERAL("file")),
              IsOk());
  EXPECT_THAT(RemoveDirectory(from), IsOk());
  EXPECT_THAT(RemoveDirectory(to), IsOk());
}

TEST(CopyTreeTest, CopiesToNewDirectory) {
  const absl::StatusOr<NativeString> temp =
      ToNative(::testing::TempDir(), Encoding::kAscii);
  ASSERT_THAT(temp, IsOkAndHolds(Not(IsEmpty())));

  const NativeString from =
      *temp + kSeparator + RULES_ELISP_NATIVE_LITERAL("from");
  const NativeString to = *temp + kSeparator + RULES_ELISP_NATIVE_LITERAL("to");
  EXPECT_THAT(CreateDirectory(from), IsOk());
  EXPECT_THAT(WriteFile(from + kSeparator + RULES_ELISP_NATIVE_LITERAL("file"),
                        "contents"),
              IsOk());

  EXPECT_THAT(CopyTree(from, to), IsOk());

  EXPECT_THAT(ReadFile(to + kSeparator + RULES_ELISP_NATIVE_LITERAL("file")),
              IsOkAndHolds("contents"));

  EXPECT_THAT(Unlink(from + kSeparator + RULES_ELISP_NATIVE_LITERAL("file")),
              IsOk());
  EXPECT_THAT(RemoveDirectory(from), IsOk());
  EXPECT_THAT(Unlink(to + kSeparator + RULES_ELISP_NATIVE_LITERAL("file")),
              IsOk());
  EXPECT_THAT(RemoveDirectory(to), IsOk());
}

TEST(CopyTreeTest, IgnoresTrailingSlash) {
  const absl::StatusOr<NativeString> temp =
      ToNative(::testing::TempDir(), Encoding::kAscii);
  ASSERT_THAT(temp, IsOkAndHolds(Not(IsEmpty())));

  const NativeString from =
      *temp + kSeparator + RULES_ELISP_NATIVE_LITERAL("from") + kSeparator;
  const NativeString to = *temp + kSeparator + RULES_ELISP_NATIVE_LITERAL("to");
  EXPECT_THAT(CreateDirectory(from), IsOk());
  EXPECT_THAT(WriteFile(from + kSeparator + RULES_ELISP_NATIVE_LITERAL("file"),
                        "contents"),
              IsOk());

  EXPECT_THAT(CopyTree(from, to), IsOk());

  EXPECT_THAT(ReadFile(to + kSeparator + RULES_ELISP_NATIVE_LITERAL("file")),
              IsOkAndHolds("contents"));

  EXPECT_THAT(Unlink(from + kSeparator + RULES_ELISP_NATIVE_LITERAL("file")),
              IsOk());
  EXPECT_THAT(RemoveDirectory(from), IsOk());
  EXPECT_THAT(Unlink(to + kSeparator + RULES_ELISP_NATIVE_LITERAL("file")),
              IsOk());
  EXPECT_THAT(RemoveDirectory(to), IsOk());
}

TEST(EnvironmentTest, CurrentReturnsValidEnv) {
  EXPECT_THAT(Environment::Current(),
              IsOkAndHolds(Contains(Pair(
                  RULES_ELISP_NATIVE_LITERAL("TEST_SRCDIR"), Not(IsEmpty())))));
}

TEST(EnvironmentTest, CreateRejectsEmpty) {
  const std::vector<std::pair<NativeString, NativeString>> pairs = {
      {RULES_ELISP_NATIVE_LITERAL("foo"), RULES_ELISP_NATIVE_LITERAL("bar")},
      {RULES_ELISP_NATIVE_LITERAL(""), RULES_ELISP_NATIVE_LITERAL("baz")},
  };
  EXPECT_THAT(Environment::Create(pairs.cbegin(), pairs.cend()),
              StatusIs(absl::StatusCode::kInvalidArgument));
}

TEST(EnvironmentTest, CreateRejectsDuplicates) {
  std::vector<std::pair<NativeString, NativeString>> pairs = {
      {RULES_ELISP_NATIVE_LITERAL("foo"), RULES_ELISP_NATIVE_LITERAL("bar")},
      {RULES_ELISP_NATIVE_LITERAL("foo"), RULES_ELISP_NATIVE_LITERAL("baz")},
  };
  EXPECT_THAT(Environment::Create(pairs.cbegin(), pairs.cend()),
              StatusIs(absl::StatusCode::kAlreadyExists));
  if constexpr (kWindows) {
    pairs.emplace_back(RULES_ELISP_NATIVE_LITERAL("Foo"),
                       RULES_ELISP_NATIVE_LITERAL("Bar"));
    EXPECT_THAT(Environment::Create(pairs.cbegin(), pairs.cend()),
              StatusIs(absl::StatusCode::kAlreadyExists));
  }
}

TEST(EnvironmentTest, CreateIsCaseSensitiveOnlyOnUnix) {
  const std::vector<std::pair<NativeString, NativeString>> pairs = {
      {RULES_ELISP_NATIVE_LITERAL("foo"), RULES_ELISP_NATIVE_LITERAL("bar")},
      {RULES_ELISP_NATIVE_LITERAL("Foo"), RULES_ELISP_NATIVE_LITERAL("baz")},
  };
  const absl::StatusOr<Environment> env =
      Environment::Create(pairs.cbegin(), pairs.cend());
  if constexpr (kWindows) {
    EXPECT_THAT(env, StatusIs(absl::StatusCode::kAlreadyExists));
  } else {
    EXPECT_THAT(env, IsOkAndHolds(SizeIs(pairs.size())));
  }
}

TEST(EnvironmentTest, GetIsCaseSensitiveOnlyOnUnix) {
  const std::vector<std::pair<NativeString, NativeString>> pairs = {
      {RULES_ELISP_NATIVE_LITERAL("foo"), RULES_ELISP_NATIVE_LITERAL("bar")},
  };
  absl::StatusOr<Environment> env =
      Environment::Create(pairs.cbegin(), pairs.cend());
  ASSERT_THAT(env, IsOkAndHolds(SizeIs(pairs.size())));
  EXPECT_EQ(env->Get(RULES_ELISP_NATIVE_LITERAL("foo")),
            RULES_ELISP_NATIVE_LITERAL("bar"));
  EXPECT_EQ(env->Get(RULES_ELISP_NATIVE_LITERAL("Foo")),
            kWindows ? RULES_ELISP_NATIVE_LITERAL("bar")
                     : RULES_ELISP_NATIVE_LITERAL(""));
}

TEST(EnvironmentTest, AddIsCaseSensitiveOnlyOnUnix) {
  const std::vector<std::pair<NativeString, NativeString>> pairs = {
      {RULES_ELISP_NATIVE_LITERAL("foo"), RULES_ELISP_NATIVE_LITERAL("bar")},
  };
  absl::StatusOr<Environment> env =
      Environment::Create(pairs.cbegin(), pairs.cend());
  ASSERT_THAT(env, IsOkAndHolds(SizeIs(pairs.size())));
  env->Add(RULES_ELISP_NATIVE_LITERAL("Foo"),
           RULES_ELISP_NATIVE_LITERAL("baz"));
  EXPECT_THAT(*env, SizeIs(pairs.size() + (kWindows ? 0 : 1)));
}

TEST(EnvironmentTest, RemoveIsCaseSensitiveOnlyOnUnix) {
  const std::vector<std::pair<NativeString, NativeString>> pairs = {
      {RULES_ELISP_NATIVE_LITERAL("foo"), RULES_ELISP_NATIVE_LITERAL("bar")},
  };
  absl::StatusOr<Environment> env =
      Environment::Create(pairs.cbegin(), pairs.cend());
  ASSERT_THAT(env, IsOkAndHolds(SizeIs(pairs.size())));
  env->Remove(RULES_ELISP_NATIVE_LITERAL("Foo"));
  EXPECT_THAT(*env, SizeIs(pairs.size() - (kWindows ? 1 : 0)));
}

TEST(TemporaryFileTest, CreateWorks) {
  absl::StatusOr<TemporaryFile> file = TemporaryFile::Create();
  ASSERT_THAT(file, IsOk());
  ASSERT_THAT(file->name(), Not(IsEmpty()));

  EXPECT_THAT(file->Write("Foo\n"), IsOk());

  std::ifstream stream(file->name(), std::ios::in | std::ios::binary);
  EXPECT_TRUE(stream.is_open());
  EXPECT_TRUE(stream.good());
  std::string line;
  std::getline(stream, line);
  EXPECT_TRUE(stream.good());
  EXPECT_EQ(line, "Foo");
}

[[maybe_unused]] static NativeString GetEnv(const NativeChar* const name) {
  const NativeChar* const absl_nullable value =
#ifdef _WIN32
      _wgetenv(name)
#else
      std::getenv(name)
#endif
      ;
  return value == nullptr ? NativeString() : NativeString(value);
}

TEST(RunTest, ReturnsExitCode) {
  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(ExecutableKind::kTest, BAZEL_CURRENT_REPOSITORY, {});
  ASSERT_THAT(runfiles, IsOk());
  const absl::StatusOr<NativeString> helper =
      runfiles->Resolve(RULES_ELISP_HELPER);
  ASSERT_THAT(helper, IsOkAndHolds(Not(IsEmpty())));

  EXPECT_THAT(
      rules_elisp::Run(*helper, {RULES_ELISP_NATIVE_LITERAL("--exit=0")}, {}),
      IsOkAndHolds(0));
  EXPECT_THAT(
      rules_elisp::Run(*helper, {RULES_ELISP_NATIVE_LITERAL("--exit=23")}, {}),
      IsOkAndHolds(23));
}

TEST(RunTest, SupportsDeadlineOnWindows) {
  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(ExecutableKind::kTest, BAZEL_CURRENT_REPOSITORY, {});
  ASSERT_THAT(runfiles, IsOk());
  const absl::StatusOr<NativeString> helper =
      runfiles->Resolve(RULES_ELISP_HELPER);
  ASSERT_THAT(helper, IsOkAndHolds(Not(IsEmpty())));

  if constexpr (kWindows) {
    RunOptions options;
    options.deadline = absl::Now() + absl::Seconds(1);
    EXPECT_THAT(
        rules_elisp::Run(*helper, {RULES_ELISP_NATIVE_LITERAL("--sleep=1m")},
                         {}, options),
        StatusIs(absl::StatusCode::kDeadlineExceeded));
  }
}

TEST(RunTest, AllowsChangingDirectory) {
  const absl::StatusOr<NativeString> temp =
      ToNative(::testing::TempDir(), Encoding::kAscii);
  ASSERT_THAT(temp, IsOkAndHolds(Not(IsEmpty())));

  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(ExecutableKind::kTest, BAZEL_CURRENT_REPOSITORY, {});
  ASSERT_THAT(runfiles, IsOk());
  const absl::StatusOr<NativeString> helper =
      runfiles->Resolve(RULES_ELISP_HELPER);
  ASSERT_THAT(helper, IsOkAndHolds(Not(IsEmpty())));

  RunOptions options;
  options.directory = *temp;

  EXPECT_THAT(rules_elisp::Run(*helper, {}, {}, options), IsOkAndHolds(0));
}

TEST(RunTest, ChangesWorkingDirectoryAndRedirectsOutput) {
  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(ExecutableKind::kTest, BAZEL_CURRENT_REPOSITORY, {});
  ASSERT_THAT(runfiles, IsOk());

  const absl::StatusOr<NativeString> helper =
      runfiles->Resolve(RULES_ELISP_HELPER);
  ASSERT_THAT(helper, IsOk());

  absl::StatusOr<NativeString> dir =
      ToNative(::testing::TempDir(), Encoding::kAscii);
  ASSERT_THAT(dir, IsOkAndHolds(Not(IsEmpty())));
  if constexpr (kWindows) {
    absl::c_replace(*dir, RULES_ELISP_NATIVE_LITERAL('/'), kSeparator);
  }
  if (dir->back() == kSeparator) dir->pop_back();

  const std::pair<NativeString, NativeString> env_vars[] = {
      // Avoid misleading warnings about unset COVERAGE_DIR and GOCOVERDIR.
      {RULES_ELISP_NATIVE_LITERAL("COVERAGE_DIR"), *dir},
      {RULES_ELISP_NATIVE_LITERAL("GOCOVERDIR"), *dir},
  };
  const absl::StatusOr<Environment> env =
      Environment::Create(std::cbegin(env_vars), std::cend(env_vars));
  ASSERT_THAT(env, IsOk());
  RunOptions options;
  options.directory = *dir;
  options.output_file =
      *dir + kSeparator + RULES_ELISP_NATIVE_LITERAL("output.log");
  EXPECT_THAT(rules_elisp::Run(*helper, {}, *env, options), IsOkAndHolds(0));

  std::ifstream stream(options.output_file, std::ios::in | std::ios::binary);
  EXPECT_TRUE(stream.is_open());
  EXPECT_TRUE(stream.good());
  stream.imbue(std::locale::classic());
  std::ostringstream buffer;
  EXPECT_TRUE(buffer.good());
  buffer.imbue(std::locale::classic());
  EXPECT_TRUE(buffer << stream.rdbuf());
  EXPECT_TRUE(stream.good());
  stream.close();
  EXPECT_TRUE(buffer.good());
  EXPECT_TRUE(buffer.flush());
  const absl::StatusOr<std::string> narrow = ToNarrow(*dir, Encoding::kUtf8);
  ASSERT_THAT(narrow, IsOk());
  EXPECT_EQ(buffer.str(), *narrow);
}

TEST(DosDeviceTest, CreatesDevice) {
  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(ExecutableKind::kTest, BAZEL_CURRENT_REPOSITORY, {});
  ASSERT_THAT(runfiles, IsOk());
  const absl::StatusOr<NativeString> file = runfiles->Resolve(RULES_ELISP_DATA);
  ASSERT_THAT(file, IsOk());
  const NativeString::size_type i =
      file->rfind(kWindows ? RULES_ELISP_NATIVE_LITERAL('\\')
                           : RULES_ELISP_NATIVE_LITERAL('/'));
  ASSERT_NE(i, file->npos);
  const NativeString dir = file->substr(0, i + 1);
  absl::StatusOr<DosDevice> dev = DosDevice::Create(dir);
  if constexpr (kWindows) {
    EXPECT_THAT(dir, SizeIs(Ge(3)));
    ASSERT_THAT(dev, IsOk());
    const NativeString name = dev->name();
    ASSERT_THAT(name, SizeIs(2));
    ASSERT_THAT(CheckAscii(name), IsOk());
    const std::optional<unsigned char> drive =
        CastNumber<unsigned char>(name.at(0));
    ASSERT_NE(drive, std::nullopt);
    EXPECT_TRUE(absl::ascii_isalpha(*drive));
    EXPECT_EQ(name.at(1), RULES_ELISP_NATIVE_LITERAL(':'));
    const std::ifstream stream(name + file->substr(i),
                               std::ios::in | std::ios::binary);
    EXPECT_TRUE(stream.is_open());
    EXPECT_TRUE(stream.good());
  }
}

}  // namespace rules_elisp
