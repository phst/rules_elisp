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
#include <iterator>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "absl/base/nullability.h"
#include "absl/cleanup/cleanup.h"
#include "absl/log/check.h"
#include "absl/status/status.h"
#include "absl/status/status_matchers.h"
#include "absl/status/statusor.h"
#include "absl/strings/ascii.h"
#include "absl/strings/str_format.h"
#include "absl/time/clock.h"
#include "absl/time/time.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

#include "elisp/private/tools/numeric.h"
#include "elisp/private/tools/platform.h"
#include "elisp/private/tools/runfiles.h"
#include "elisp/private/tools/strings.h"

namespace rules_elisp {
namespace {

using absl_testing::IsOk;
using absl_testing::IsOkAndHolds;
using absl_testing::StatusIs;
using ::testing::AnyOf;
using ::testing::Contains;
using ::testing::ElementsAre;
using ::testing::EndsWith;
using ::testing::Ge;
using ::testing::Gt;
using ::testing::IsEmpty;
using ::testing::Not;
using ::testing::Pair;
using ::testing::PrintToString;
using ::testing::SizeIs;
using ::testing::StartsWith;
using ::testing::TestWithParam;
using ::testing::Values;

static absl::StatusOr<FileName> TempDir() {
  const absl::StatusOr<NativeString> native =
      ToNative(::testing::TempDir(), Encoding::kAscii);
  if (!native.ok()) return native.status();
  return FileName::FromString(*native);
}

TEST(FileNameTest, RejectsEmpty) {
  EXPECT_THAT(FileName::FromString(RULES_ELISP_NATIVE_LITERAL("")),
              StatusIs(absl::StatusCode::kInvalidArgument));
}

TEST(FileNameTest, RejectsNull) {
  static constexpr NativeChar chars[] =
      RULES_ELISP_NATIVE_LITERAL("foo \x00 bar");
  constexpr std::size_t size = std::size(chars);
  static_assert(size > 0);
  constexpr std::size_t length = size - 1;
  static_assert(chars[length] == RULES_ELISP_NATIVE_LITERAL('\0'));
  constexpr NativeStringView name(chars, length);
  static_assert(name.find(RULES_ELISP_NATIVE_LITERAL('\0')) != name.npos);

  EXPECT_THAT(FileName::FromString(name),
              StatusIs(absl::StatusCode::kInvalidArgument));
}

TEST(FileNameTest, AcceptsRootLocalDevice) {
  // https://googleprojectzero.blogspot.com/2016/02/the-definitive-guide-on-win32-to-nt.html
  EXPECT_THAT(
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("\\\\?\\X:\\ABC\\DEF")),
      IsOk());
  EXPECT_THAT(FileName::FromString(RULES_ELISP_NATIVE_LITERAL("\\\\?\\X:\\")),
              IsOk());
}

class FileNameTest : public TestWithParam<NativeStringView> {};

TEST_P(FileNameTest, RejectsExoticNamesOnWindows) {
  if constexpr (kWindows) {
    // See
    // https://googleprojectzero.blogspot.com/2016/02/the-definitive-guide-on-win32-to-nt.html
    // for an exhaustive description of Windows filenames.  We want to reject
    // all of them except relative MS-DOS-style drive absolute, and root-local
    // device ones.
    EXPECT_THAT(FileName::FromString(this->GetParam()),
                StatusIs(absl::StatusCode::kInvalidArgument));
  }
}

INSTANTIATE_TEST_SUITE_P(
    ExoticFileNames, FileNameTest,
    Values(
        // Drive Relative
        RULES_ELISP_NATIVE_LITERAL("X:DEF\\GHI"),
        RULES_ELISP_NATIVE_LITERAL("X:"),
        RULES_ELISP_NATIVE_LITERAL("X:DEF. ."),
        RULES_ELISP_NATIVE_LITERAL("X:ABC\\..\\..\\.."),
        // Rooted
        RULES_ELISP_NATIVE_LITERAL("\\ABC\\DEF"),
        RULES_ELISP_NATIVE_LITERAL("\\"),
        RULES_ELISP_NATIVE_LITERAL("\\ABC\\DEF. ."),
        RULES_ELISP_NATIVE_LITERAL("/ABC/DEF"),
        // UNC Absolute
        RULES_ELISP_NATIVE_LITERAL("\\ABC\\..\\..\\.."),
        RULES_ELISP_NATIVE_LITERAL("\\\\server\\share\\ABC\\DEF"),
        RULES_ELISP_NATIVE_LITERAL("\\\\server"),
        RULES_ELISP_NATIVE_LITERAL("\\\\server\\share"),
        RULES_ELISP_NATIVE_LITERAL("//server/share/ABC/DEF"),
        RULES_ELISP_NATIVE_LITERAL("\\\\;LanmanRedirector\\evil.com\\xyz"),
        // Local Device
        RULES_ELISP_NATIVE_LITERAL("\\\\.\\COM20"),
        RULES_ELISP_NATIVE_LITERAL("\\\\.\\pipe\\mypipe"),
        RULES_ELISP_NATIVE_LITERAL("\\\\.\\X:\\ABC\\DEF. ."),
        RULES_ELISP_NATIVE_LITERAL("\\\\.\\X:/ABC/DEF"),
        RULES_ELISP_NATIVE_LITERAL("\\\\.\\X:\\ABC\\..\\..\\C:\\"),
        // Root Local Device
        RULES_ELISP_NATIVE_LITERAL("\\\\?\\X:"),
        RULES_ELISP_NATIVE_LITERAL("\\\\?\\X:/ABC/DEF"),
        RULES_ELISP_NATIVE_LITERAL("\\\\?\\X:\\ABC\\..\\..\\.."),
        RULES_ELISP_NATIVE_LITERAL("\\??\\X:\\ABC\\DEF"),
        RULES_ELISP_NATIVE_LITERAL("\\??\\X:"),
        RULES_ELISP_NATIVE_LITERAL("\\??\\X:/ABC/DEF")));

TEST(FileNameTest, IsFormattable) {
  const absl::StatusOr<FileName> foo =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("foo"));
  const absl::StatusOr<FileName> bar =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("bar äα𝐴🐈'"));
  EXPECT_EQ(absl::StrFormat("bar %s baz", foo.value()), "bar foo baz");
  EXPECT_EQ(absl::StrFormat("foo %s baz", bar.value()), "foo bar äα𝐴🐈' baz");
  EXPECT_EQ(absl::StrFormat("%s", bar.value()), "bar äα𝐴🐈'");
  EXPECT_EQ(absl::StrFormat("%#s", bar.value()), R"*("bar äα𝐴🐈'")*");
  EXPECT_EQ(PrintToString(foo.value()), "foo");
  EXPECT_EQ(PrintToString(bar.value()), "bar äα𝐴🐈'");
  // The following should work, but doesn’t due to
  // https://github.com/abseil/abseil-cpp/issues/1966.
  // EXPECT_EQ(PrintToString(foo), "foo");
  // EXPECT_EQ(PrintToString(bar), "bar äα𝐴🐈'");
}

TEST(FileNameTest, ParentRejectsRoot) {
  EXPECT_THAT(FileName::FromString(kWindows ? RULES_ELISP_NATIVE_LITERAL("C:\\")
                                            : RULES_ELISP_NATIVE_LITERAL("/"))
                  .value()
                  .Parent(),
              StatusIs(absl::StatusCode::kInvalidArgument));
}

TEST(FileNameTest, ParentRejectsDot) {
  EXPECT_THAT(FileName::FromString(RULES_ELISP_NATIVE_LITERAL("foo/."))
                  .value()
                  .Parent(),
              StatusIs(absl::StatusCode::kInvalidArgument));
}

TEST(FileNameTest, ParentRejectsDotDot) {
  EXPECT_THAT(FileName::FromString(RULES_ELISP_NATIVE_LITERAL("foo/.."))
                  .value()
                  .Parent(),
              StatusIs(absl::StatusCode::kInvalidArgument));
}

TEST(FileNameTest, ParentWorksForRelativeName) {
  EXPECT_THAT(
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("foo/bar"))
          .value()
          .Parent(),
      IsOkAndHolds(
          FileName::FromString(RULES_ELISP_NATIVE_LITERAL("foo")).value()));
}

TEST(FileNameTest, ParentWorksForAbsoluteName) {
  EXPECT_THAT(
      FileName::FromString(kWindows ? RULES_ELISP_NATIVE_LITERAL("C:\\Foo")
                                    : RULES_ELISP_NATIVE_LITERAL("/foo"))
          .value()
          .Parent(),
      IsOkAndHolds(FileName::FromString(kWindows
                                            ? RULES_ELISP_NATIVE_LITERAL("C:\\")
                                            : RULES_ELISP_NATIVE_LITERAL("/"))
                       .value()));
  EXPECT_THAT(
      FileName::FromString(kWindows ? RULES_ELISP_NATIVE_LITERAL("C:\\Foo\\Bar")
                                    : RULES_ELISP_NATIVE_LITERAL("/foo/bar"))
          .value()
          .Parent(),
      IsOkAndHolds(
          FileName::FromString(kWindows ? RULES_ELISP_NATIVE_LITERAL("C:\\Foo")
                                        : RULES_ELISP_NATIVE_LITERAL("/foo"))
              .value()));
}

TEST(FileNameTest, ParentIgnoresTrailingSlashes) {
  EXPECT_THAT(
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("foo/bar//"))
          .value()
          .Parent(),
      IsOkAndHolds(
          FileName::FromString(RULES_ELISP_NATIVE_LITERAL("foo")).value()));
}

TEST(FileNameTest, ChildRejectsDescendant) {
  const FileName parent =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("foo")).value();
  EXPECT_THAT(parent.Child(RULES_ELISP_NATIVE_LITERAL("bar/baz")),
              StatusIs(absl::StatusCode::kInvalidArgument));
}

TEST(FileNameTest, ChildRejectsAbsolute) {
  const FileName parent =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("foo")).value();
  EXPECT_THAT(parent.Child(RULES_ELISP_NATIVE_LITERAL("/bar")),
              StatusIs(absl::StatusCode::kInvalidArgument));
}

TEST(FileNameTest, ChildCoalescesSlashes) {
  const FileName parent =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("foo/")).value();
  const absl::StatusOr<FileName> child =
      parent.Child(RULES_ELISP_NATIVE_LITERAL("bar"));
  ASSERT_THAT(child, IsOk());
  EXPECT_EQ(child->string(), kWindows ? RULES_ELISP_NATIVE_LITERAL("foo\\bar")
                                      : RULES_ELISP_NATIVE_LITERAL("foo/bar"));
}

TEST(FileNameTest, JoinRejectsAbsolute) {
  const FileName parent =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("foo")).value();
  EXPECT_THAT(parent.Child(RULES_ELISP_NATIVE_LITERAL("/bar/baz")),
              StatusIs(absl::StatusCode::kInvalidArgument));
}

TEST(FileNameTest, JoinCoalescesSlashes) {
  const FileName parent =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("foo/")).value();
  const absl::StatusOr<FileName> descendant =
      parent.Join(RULES_ELISP_NATIVE_LITERAL("bar/baz"));
  ASSERT_THAT(descendant, IsOk());
  EXPECT_EQ(descendant->string(),
            kWindows ? RULES_ELISP_NATIVE_LITERAL("foo\\bar\\baz")
                     : RULES_ELISP_NATIVE_LITERAL("foo/bar/baz"));
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
  bool valid;
  bool absolute;

  template <typename Sink>
  friend void AbslStringify(Sink& sink, const IsAbsoluteParam& param) {
    absl::Format(
        &sink, "Filename %s should be %s", param.name,
        param.valid ? (param.absolute ? "absolute" : "relative") : "invalid");
  }
};

class IsAbsoluteTest : public TestWithParam<IsAbsoluteParam> {};

TEST_P(IsAbsoluteTest, Works) {
  const IsAbsoluteParam& param = this->GetParam();
  const absl::StatusOr<FileName> filename = FileName::FromString(param.name);
  if (param.valid) {
    ASSERT_THAT(filename, IsOk());
    EXPECT_EQ(filename->IsAbsolute(), param.absolute);
  } else {
    EXPECT_THAT(filename, StatusIs(absl::StatusCode::kInvalidArgument));
  }
}

INSTANTIATE_TEST_SUITE_P(
    VariousFilenames, IsAbsoluteTest,
    Values(
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL(""), false, false},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("."), true, false},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("foo"), true, false},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("foo/bar"), true, false},
        // See
        // https://googleprojectzero.blogspot.com/2016/02/the-definitive-guide-on-win32-to-nt.html
        // for the various kinds of filenames on Windows.
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("C:\\"), true, kWindows},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("C:/"), true, kWindows},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("C:\\Foo"), true, kWindows},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("C:/Foo"), true, kWindows},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("C:"), !kWindows, false},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("C:Foo"), !kWindows, false},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("\\Foo"), !kWindows, false},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("/Foo"), !kWindows,
                        !kWindows},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("NUL"), true, false},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("/"), !kWindows, !kWindows},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("/foo"), !kWindows,
                        !kWindows},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("\\\\?\\X:\\ABC\\DEF"), true,
                        kWindows},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("\\\\?\\X:\\"), true,
                        kWindows},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("\\\\?\\X:"), !kWindows,
                        kWindows},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("\\\\?\\X:\\ABC\\DEF. ."),
                        !kWindows, kWindows},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("\\\\?\\X:/ABC/DEF"),
                        !kWindows, kWindows},
        IsAbsoluteParam{RULES_ELISP_NATIVE_LITERAL("\\\\?\\X:\\ABC\\..\\XYZ"),
                        !kWindows, kWindows},
        IsAbsoluteParam{
            RULES_ELISP_NATIVE_LITERAL("\\\\?\\X:\\ABC\\..\\..\\.."), !kWindows,
            kWindows}));

TEST(MakeAbsoluteTest, KeepsAbsoluteName) {
  const absl::StatusOr<FileName> file = FileName::FromString(
      kWindows ? RULES_ELISP_NATIVE_LITERAL("C:\\Foo\\Bar.txt")
               : RULES_ELISP_NATIVE_LITERAL("/foo/bar.txt"));
  ASSERT_THAT(file, IsOk());
  EXPECT_THAT(file->MakeAbsolute(), IsOkAndHolds(*file));
}

TEST(MakeAbsoluteTest, MakesRelativeNameAbsolute) {
  const absl::StatusOr<FileName> relative =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("foo.txt"));
  ASSERT_THAT(relative, IsOk());
  const absl::StatusOr<FileName> file = relative->MakeAbsolute();
  ASSERT_THAT(file, IsOk());
  const NativeString& string = file->string();
  if constexpr (kWindows) {
    ASSERT_THAT(string, SizeIs(Gt(3)));
    const std::optional<unsigned char> drive =
        CastNumber<unsigned char>(string.at(0));
    ASSERT_NE(drive, std::nullopt);
    EXPECT_TRUE(absl::ascii_isalpha(*drive));
    EXPECT_EQ(string.at(1), RULES_ELISP_NATIVE_LITERAL(':'));
    EXPECT_EQ(string.at(2), RULES_ELISP_NATIVE_LITERAL('\\'));
    EXPECT_THAT(string, EndsWith(RULES_ELISP_NATIVE_LITERAL("\\foo.txt")));
  } else {
    EXPECT_THAT(string, StartsWith(RULES_ELISP_NATIVE_LITERAL("/")));
    EXPECT_THAT(string, EndsWith(RULES_ELISP_NATIVE_LITERAL("/foo.txt")));
  }
}

TEST(MakeRelativeTest, RejectsNotWithin) {
  const FileName foo =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("foo")).value();
  const FileName fooooo =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("fooooo")).value();
  const FileName bar =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("bar")).value();

  EXPECT_THAT(foo.MakeRelative(foo),
              StatusIs(absl::StatusCode::kInvalidArgument));
  EXPECT_THAT(foo.MakeRelative(bar),
              StatusIs(absl::StatusCode::kInvalidArgument));
  EXPECT_THAT(fooooo.MakeRelative(foo),
              StatusIs(absl::StatusCode::kInvalidArgument));
  if constexpr (!kWindows) {
    const FileName c_foo =
        FileName::FromString(RULES_ELISP_NATIVE_LITERAL("C:\\Foo")).value();
    const FileName c_foo_bar =
        FileName::FromString(RULES_ELISP_NATIVE_LITERAL("C:\\Foo\\Bar"))
            .value();
    EXPECT_THAT(c_foo_bar.MakeRelative(c_foo),
                StatusIs(absl::StatusCode::kInvalidArgument));
  }
}

TEST(MakeRelativeTest, Relativizes) {
  const FileName foo =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("foo")).value();
  const FileName foo_slash =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("foo/")).value();
  const FileName bar =
      FileName::FromString(kWindows ? RULES_ELISP_NATIVE_LITERAL(".\\bar")
                                    : RULES_ELISP_NATIVE_LITERAL("bar"))
          .value();
  const FileName foo_bar =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("foo/bar")).value();

  EXPECT_THAT(foo_bar.MakeRelative(foo), IsOkAndHolds(bar));
  EXPECT_THAT(foo_bar.MakeRelative(foo_slash), IsOkAndHolds(bar));
  if constexpr (kWindows) {
    const FileName c_foo =
        FileName::FromString(RULES_ELISP_NATIVE_LITERAL("C:\\Foo")).value();
    const FileName c_foo_bar =
        FileName::FromString(RULES_ELISP_NATIVE_LITERAL("C:\\Foo\\Bar"))
            .value();
    const FileName c_bar =
        FileName::FromString(RULES_ELISP_NATIVE_LITERAL(".\\Bar")).value();
    EXPECT_THAT(c_foo_bar.MakeRelative(c_foo), IsOkAndHolds(c_bar));
  }
}

TEST(FileNameTest, ResolveRejectsNonExisting) {
  const absl::StatusOr<FileName> dir = TempDir();
  ASSERT_THAT(dir, IsOk());

  const FileName file =
      dir->Child(RULES_ELISP_NATIVE_LITERAL("nonexisting")).value();

  EXPECT_THAT(file.Resolve(), StatusIs(absl::StatusCode::kNotFound));
}

TEST(FileNameTest, ResolvesRegularFile) {
  const absl::StatusOr<FileName> dir = TempDir();
  ASSERT_THAT(dir, IsOk());

  const FileName file = dir->Child(RULES_ELISP_NATIVE_LITERAL("file")).value();
  EXPECT_THAT(WriteFile(file, "contents"), IsOk());
  const absl::Cleanup cleanup = [&file] { EXPECT_THAT(Unlink(file), IsOk()); };

  const absl::StatusOr<FileName> resolved = file.Resolve();
  ASSERT_THAT(resolved, IsOk());
  EXPECT_THAT(ReadFile(*resolved), IsOkAndHolds("contents"));
}

TEST(FileNameTest, ResolvesDirectory) {
  const absl::StatusOr<FileName> dir = TempDir();
  ASSERT_THAT(dir, IsOk());

  EXPECT_THAT(dir->Resolve(), IsOk());
}

TEST(FileExistsTest, TestsThatFileExists) {
  const absl::StatusOr<FileName> dir = TempDir();
  ASSERT_THAT(dir, IsOk());

  EXPECT_TRUE(FileExists(*dir));

  const FileName child =
      dir->Child(RULES_ELISP_NATIVE_LITERAL("nonexistent")).value();
  EXPECT_FALSE(FileExists(child));
}

TEST(ReadWriteFileTest, Works) {
  const absl::StatusOr<FileName> dir = TempDir();
  ASSERT_THAT(dir, IsOk());

  const FileName file = dir->Child(RULES_ELISP_NATIVE_LITERAL("file")).value();
  EXPECT_FALSE(FileExists(file));

  EXPECT_THAT(WriteFile(file, "contents"), IsOk());

  EXPECT_TRUE(FileExists(file));

  const absl::Cleanup cleanup = [&file] { EXPECT_THAT(Unlink(file), IsOk()); };

  EXPECT_THAT(ReadFile(file), IsOkAndHolds("contents"));
}

TEST(IsNonEmptyDirectoryTest, ReturnsFalseForFile) {
  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(ExecutableKind::kTest, BAZEL_CURRENT_REPOSITORY, {});
  ASSERT_THAT(runfiles, IsOk());
  const absl::StatusOr<FileName> file = runfiles->Resolve(RULES_ELISP_DATA);
  ASSERT_THAT(file, IsOk());

  EXPECT_FALSE(IsNonEmptyDirectory(*file));
}

TEST(IsNonEmptyDirectoryTest, ReturnsTrueForEmptyDirectory) {
  const absl::StatusOr<FileName> temp = TempDir();
  ASSERT_THAT(temp, IsOk());

  const FileName dir =
      temp->Child(RULES_ELISP_NATIVE_LITERAL("nonempty-dir")).value();
  EXPECT_THAT(CreateDirectory(dir), IsOk());

  EXPECT_TRUE(IsNonEmptyDirectory(*temp));
  EXPECT_FALSE(IsNonEmptyDirectory(dir));
}

TEST(IsNonEmptyDirectoryTest, ReturnsFalseForNonEmptyDirectory) {
  const absl::StatusOr<FileName> temp = TempDir();
  ASSERT_THAT(temp, IsOk());

  const FileName dir =
      temp->Child(RULES_ELISP_NATIVE_LITERAL("empty-dir")).value();
  EXPECT_THAT(CreateDirectory(dir), IsOk());

  EXPECT_TRUE(IsNonEmptyDirectory(*temp));
  EXPECT_FALSE(IsNonEmptyDirectory(dir));

  const FileName file =
      dir.Child(RULES_ELISP_NATIVE_LITERAL("file.txt")).value();
  EXPECT_THAT(WriteFile(file, ""), IsOk());

  EXPECT_TRUE(IsNonEmptyDirectory(dir));
}

TEST(UnlinkTest, RemovesFile) {
  const absl::StatusOr<FileName> temp = TempDir();
  ASSERT_THAT(temp, IsOk());

  const FileName file =
      temp->Child(RULES_ELISP_NATIVE_LITERAL("unlink-test")).value();

  EXPECT_FALSE(FileExists(file));
  EXPECT_THAT(Unlink(file), StatusIs(absl::StatusCode::kNotFound));

  EXPECT_THAT(WriteFile(file, ""), IsOk());

  EXPECT_TRUE(FileExists(file));
  EXPECT_THAT(Unlink(file), IsOk());
  EXPECT_FALSE(FileExists(file));
  EXPECT_THAT(Unlink(file), StatusIs(absl::StatusCode::kNotFound));
  EXPECT_FALSE(FileExists(file));
}

TEST(CreateRemoveDirectoryTest, CreatesAndRemovesDirectories) {
  const absl::StatusOr<FileName> temp = TempDir();
  ASSERT_THAT(temp, IsOk());

  const FileName dir = temp->Child(RULES_ELISP_NATIVE_LITERAL("dir")).value();
  EXPECT_THAT(CreateDirectory(dir), IsOk());

  const FileName file = dir.Child(RULES_ELISP_NATIVE_LITERAL("file")).value();
  EXPECT_THAT(WriteFile(file, ""), IsOk());

  EXPECT_THAT(RemoveDirectory(dir),
              StatusIs(absl::StatusCode::kFailedPrecondition));
  EXPECT_THAT(Unlink(file), IsOk());
  EXPECT_THAT(RemoveDirectory(dir), IsOk());
}

TEST(CreateDirectoriesTest, LeavesExistingDirectoryAlone) {
  const absl::StatusOr<FileName> temp = TempDir();
  ASSERT_THAT(temp, IsOk());

  EXPECT_THAT(CreateDirectories(*temp), IsOk());
}

TEST(CreateDirectoriesTest, CreatesHierarchy) {
  const absl::StatusOr<FileName> temp = TempDir();
  ASSERT_THAT(temp, IsOk());

  const FileName child =
      temp->Child(RULES_ELISP_NATIVE_LITERAL("child")).value();
  const FileName grandchild =
      child.Child(RULES_ELISP_NATIVE_LITERAL("grandchild")).value();

  EXPECT_THAT(CreateDirectories(grandchild), IsOk());

  EXPECT_THAT(RemoveDirectory(grandchild), IsOk());
  EXPECT_THAT(RemoveDirectory(child), IsOk());
}

TEST(RemoveTreeTest, RemovesTree) {
  const absl::StatusOr<FileName> temp = TempDir();
  ASSERT_THAT(temp, IsOk());

  const FileName dir = temp->Child(RULES_ELISP_NATIVE_LITERAL("dir")).value();
  const FileName file = dir.Child(RULES_ELISP_NATIVE_LITERAL("file")).value();

  EXPECT_THAT(CreateDirectory(dir), IsOk());
  EXPECT_THAT(WriteFile(file, "contents"), IsOk());

  EXPECT_THAT(RemoveTree(dir), IsOk());

  EXPECT_FALSE(FileExists(dir));
  EXPECT_FALSE(FileExists(file));
}

TEST(ListDirectoryTests, ListsDirectory) {
  const absl::StatusOr<FileName> temp = TempDir();
  ASSERT_THAT(temp, IsOk());

  const FileName dir =
      temp->Child(RULES_ELISP_NATIVE_LITERAL("dir äα𝐴🐈'")).value();
  const FileName element =
      FileName::FromString(RULES_ELISP_NATIVE_LITERAL("file äα𝐴🐈'")).value();

  EXPECT_THAT(ListDirectory(dir, RULES_ELISP_NATIVE_LITERAL("*")),
              StatusIs(absl::StatusCode::kNotFound));
  EXPECT_THAT(CreateDirectory(dir), IsOk());
  EXPECT_THAT(ListDirectory(dir, RULES_ELISP_NATIVE_LITERAL("*")), IsOkAndHolds(IsEmpty()));

  const FileName file =
      dir.Child(RULES_ELISP_NATIVE_LITERAL("file äα𝐴🐈'")).value();
  EXPECT_THAT(WriteFile(file, ""), IsOk());

  EXPECT_THAT(ListDirectory(dir, RULES_ELISP_NATIVE_LITERAL("*")),
              IsOkAndHolds(ElementsAre(element)));
  EXPECT_THAT(ListDirectory(dir, RULES_ELISP_NATIVE_LITERAL("*i?? *")),
              IsOkAndHolds(ElementsAre(element)));

  EXPECT_THAT(Unlink(file), IsOk());
  EXPECT_THAT(ListDirectory(dir, RULES_ELISP_NATIVE_LITERAL("*")),
              IsOkAndHolds(IsEmpty()));
  EXPECT_THAT(RemoveDirectory(dir), IsOk());
  EXPECT_THAT(ListDirectory(dir, RULES_ELISP_NATIVE_LITERAL("*")),
              StatusIs(absl::StatusCode::kNotFound));
}

TEST(RenameTest, RenamesFile) {
  const absl::StatusOr<FileName> temp = TempDir();
  ASSERT_THAT(temp, IsOk());

  const FileName from = temp->Child(RULES_ELISP_NATIVE_LITERAL("from")).value();
  const FileName to = temp->Child(RULES_ELISP_NATIVE_LITERAL("to")).value();

  EXPECT_THAT(WriteFile(from, "contents"), IsOk());

  EXPECT_THAT(Rename(from, to), IsOk());

  EXPECT_FALSE(FileExists(from));
  EXPECT_THAT(ReadFile(to), IsOkAndHolds("contents"));

  EXPECT_THAT(Unlink(to), IsOk());
}

TEST(CopyFileTest, RejectsDirectory) {
  const absl::StatusOr<FileName> temp = TempDir();
  ASSERT_THAT(temp, IsOk());

  const FileName from = temp->Child(RULES_ELISP_NATIVE_LITERAL("from")).value();
  const FileName to = temp->Child(RULES_ELISP_NATIVE_LITERAL("to")).value();

  EXPECT_THAT(CreateDirectory(from), IsOk());
  const absl::Cleanup cleanup = [&from] {
    EXPECT_THAT(RemoveDirectory(from), IsOk());
  };

  EXPECT_THAT(CopyFile(from, to),
              StatusIs(AnyOf(absl::StatusCode::kFailedPrecondition,
                             absl::StatusCode::kPermissionDenied)));
}

TEST(CopyFileTest, RejectsNonExisting) {
  const absl::StatusOr<FileName> temp = TempDir();
  ASSERT_THAT(temp, IsOk());

  const FileName from = temp->Child(RULES_ELISP_NATIVE_LITERAL("from")).value();
  const FileName to = temp->Child(RULES_ELISP_NATIVE_LITERAL("to")).value();

  EXPECT_THAT(CopyFile(from, to), StatusIs(absl::StatusCode::kNotFound));
}

TEST(CopyFileTest, RejectsSelfCopy) {
  const absl::StatusOr<FileName> temp = TempDir();
  ASSERT_THAT(temp, IsOk());

  const FileName from = temp->Child(RULES_ELISP_NATIVE_LITERAL("from")).value();
  EXPECT_THAT(WriteFile(from, "contents"), IsOk());
  const absl::Cleanup cleanup = [&from] { EXPECT_THAT(Unlink(from), IsOk()); };

  EXPECT_THAT(CopyFile(from, from), StatusIs(absl::StatusCode::kAlreadyExists));
}

TEST(CopyFileTest, CopiesFile) {
  const absl::StatusOr<FileName> temp = TempDir();
  ASSERT_THAT(temp, IsOk());

  const FileName from = temp->Child(RULES_ELISP_NATIVE_LITERAL("from")).value();
  const FileName to = temp->Child(RULES_ELISP_NATIVE_LITERAL("to")).value();
  EXPECT_THAT(WriteFile(from, "contents"), IsOk());

  const absl::Cleanup cleanup = [&from, &to] {
    EXPECT_THAT(Unlink(from), IsOk());
    EXPECT_THAT(Unlink(to), IsOk());
  };

  EXPECT_THAT(CopyFile(from, to), IsOk());
  EXPECT_THAT(ReadFile(to), IsOkAndHolds("contents"));
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

TEST(CreateTemporaryDirectoryTest, Works) {
  absl::StatusOr<FileName> dir = CreateTemporaryDirectory();
  ASSERT_THAT(dir, IsOk());

  const absl::Cleanup cleanup = [&dir] {
    EXPECT_THAT(RemoveTree(*dir), IsOk());
  };

  FileName file = dir->Child(RULES_ELISP_NATIVE_LITERAL("file")).value();
  EXPECT_THAT(WriteFile(file, "contents"), IsOk());
  EXPECT_THAT(ReadFile(file), IsOkAndHolds("contents"));
}

TEST(SearchPathTest, FindsProgram) {
  const FileName name =
      FileName::FromString(kWindows ? RULES_ELISP_NATIVE_LITERAL("cmd")
                                    : RULES_ELISP_NATIVE_LITERAL("true"))
          .value();
  EXPECT_THAT(SearchPath(name), IsOk());
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
  const absl::StatusOr<FileName> helper = runfiles->Resolve(RULES_ELISP_HELPER);
  ASSERT_THAT(helper, IsOk());

  EXPECT_THAT(RunProcess(*helper, {RULES_ELISP_NATIVE_LITERAL("--exit=0")}, {}),
              IsOkAndHolds(0));
  EXPECT_THAT(
      RunProcess(*helper, {RULES_ELISP_NATIVE_LITERAL("--exit=23")}, {}),
      IsOkAndHolds(23));
}

TEST(RunTest, SupportsDeadlineOnWindows) {
  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(ExecutableKind::kTest, BAZEL_CURRENT_REPOSITORY, {});
  ASSERT_THAT(runfiles, IsOk());
  const absl::StatusOr<FileName> helper = runfiles->Resolve(RULES_ELISP_HELPER);
  ASSERT_THAT(helper, IsOk());

  if constexpr (kWindows) {
    ProcessOptions options;
    options.deadline = absl::Now() + absl::Seconds(1);
    EXPECT_THAT(RunProcess(*helper, {RULES_ELISP_NATIVE_LITERAL("--sleep=1m")},
                           {}, options),
                StatusIs(absl::StatusCode::kDeadlineExceeded));
  }
}

TEST(RunTest, AllowsChangingDirectory) {
  const absl::StatusOr<FileName> temp = TempDir();
  ASSERT_THAT(temp, IsOk());

  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(ExecutableKind::kTest, BAZEL_CURRENT_REPOSITORY, {});
  ASSERT_THAT(runfiles, IsOk());
  const absl::StatusOr<FileName> helper = runfiles->Resolve(RULES_ELISP_HELPER);
  ASSERT_THAT(helper, IsOk());

  ProcessOptions options;
  options.directory = *temp;

  EXPECT_THAT(RunProcess(*helper, {}, {}, options), IsOkAndHolds(0));
}

TEST(RunTest, ChangesWorkingDirectoryAndRedirectsOutput) {
  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(ExecutableKind::kTest, BAZEL_CURRENT_REPOSITORY, {});
  ASSERT_THAT(runfiles, IsOk());

  const absl::StatusOr<FileName> helper = runfiles->Resolve(RULES_ELISP_HELPER);
  ASSERT_THAT(helper, IsOk());

  const absl::StatusOr<FileName> dir = TempDir();
  ASSERT_THAT(dir, IsOk());

  const FileName output_file =
      dir->Child(RULES_ELISP_NATIVE_LITERAL("output.log")).value();

  const std::pair<NativeString, NativeString> env_vars[] = {
      // Avoid misleading warnings about unset COVERAGE_DIR and GOCOVERDIR.
      {RULES_ELISP_NATIVE_LITERAL("COVERAGE_DIR"), dir->string()},
      {RULES_ELISP_NATIVE_LITERAL("GOCOVERDIR"), dir->string()},
  };
  const absl::StatusOr<Environment> env =
      Environment::Create(std::cbegin(env_vars), std::cend(env_vars));
  ASSERT_THAT(env, IsOk());
  ProcessOptions options;
  options.directory = *dir;
  options.output_file = output_file;
  EXPECT_THAT(RunProcess(*helper, {}, *env, options), IsOkAndHolds(0));

  absl::StatusOr<std::string> narrow = ToNarrow(dir->string(), Encoding::kUtf8);
  ASSERT_THAT(narrow, IsOk());
  while (narrow->back() == kSeparator) narrow->pop_back();
  EXPECT_THAT(ReadFile(output_file), IsOkAndHolds(*narrow));
}

TEST(DosDeviceTest, CreatesDevice) {
  const absl::StatusOr<Runfiles> runfiles =
      Runfiles::Create(ExecutableKind::kTest, BAZEL_CURRENT_REPOSITORY, {});
  ASSERT_THAT(runfiles, IsOk());
  const absl::StatusOr<FileName> file = runfiles->Resolve(RULES_ELISP_DATA);
  ASSERT_THAT(file, IsOk());
  const NativeString& string = file->string();
  const NativeString::size_type i =
      string.rfind(kWindows ? RULES_ELISP_NATIVE_LITERAL('\\')
                            : RULES_ELISP_NATIVE_LITERAL('/'));
  ASSERT_NE(i, string.npos);
  const absl::StatusOr<FileName> dir =
      FileName::FromString(string.substr(0, i + 1));
  ASSERT_THAT(dir, IsOk());
  absl::StatusOr<DosDevice> dev = DosDevice::Create(*dir);
  if constexpr (kWindows) {
    EXPECT_THAT(dir->string(), SizeIs(Ge(3)));
    ASSERT_THAT(dev, IsOk());
    const NativeString name = dev->name();
    ASSERT_THAT(name, SizeIs(2));
    ASSERT_THAT(CheckAscii(name), IsOk());
    const std::optional<unsigned char> drive =
        CastNumber<unsigned char>(name.at(0));
    ASSERT_NE(drive, std::nullopt);
    EXPECT_TRUE(absl::ascii_isalpha(*drive));
    EXPECT_EQ(name.at(1), RULES_ELISP_NATIVE_LITERAL(':'));
    const absl::StatusOr<FileName> abbrev =
        FileName::FromString(name + string.substr(i));
    ASSERT_THAT(abbrev, IsOk());
    EXPECT_THAT(ReadFile(*abbrev), IsOk());
  }
}

}  // namespace
}  // namespace rules_elisp
