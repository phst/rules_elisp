// Copyright 2022, 2023, 2024, 2025 Google LLC
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

// This file implements the core of the Emacs Lisp protocol buffer support.
// It’s a dynamic module; see Info node ‘(elisp) Writing Dynamic Modules’.  The
// implementation uses the µpb library; see
// https://github.com/protocolbuffers/upb.  In particular, it uses the
// reflection-based approach described in
// https://github.com/protocolbuffers/protobuf/blob/main/docs/upb/wrapping-upb.md.
// There’s a companion library, proto.el, which contains some higher-level
// applications as well as definitions that are easier to write in Lisp than C.
// The existence of this module is an implementation detail, and users should
// only load proto.el.
//
// Protocol buffer objects (messages, arrays, maps) are represented as user
// pointers wrapped in structures of type ‘elisp/proto/object’, defined in
// proto.el.  ‘elisp/proto/object’ also contains a pointer to an arena from
// which the C object was allocated; see the section “Integrating GC with µpb”
// in
// https://github.com/protocolbuffers/protobuf/blob/main/docs/upb/wrapping-upb.md.
//
// Functions whose name starts with “Extract” convert a Lisp value into a
// corresponding C object, and functions whose name starts with “Make” perform
// the conversion in the opposite direction.  This mimics the naming conventions
// used in the Emacs module API.  Functions whose name starts with “Adopt”
// perform a looser conversion from Lisp to C; for example, they also accept
// Lisp sequences in place of µpb arrays.  These functions also fuse the
// extracted object’s arena into a destination arena, so that callers can use
// them to “adopt” unrelated Lisp values into existing protocol buffer objects.
//
// All functions take a “struct Context” object as first parameter and should
// use that to interact with Emacs and access global values.  If any error
// occurs, it should be reported using one of the “Signal” functions, and the
// function should return a sentinel value like NULL if possible.  Functions
// that take an upb_Arena* parameter should normally use that arena to allocate
// memory, unless the memory is short-lived; they assume that any other
// arena-allocated object passed to them has been allocated from the same arena.
// Other functions that allocate memory either take a struct Allocator parameter
// to use as allocator, or allocate from the normal heap.  Memory allocated from
// an arena is never freed explicitly; other memory should be freed as needed.
//
// We use the term “scalar value” in the same sense as the protocol buffer
// documentation; see
// https://developers.google.com/protocol-buffers/docs/proto3#scalar.  A field
// that is neither repeated nor a map is called “singular”; see
// https://developers.google.com/protocol-buffers/docs/proto3#specifying_field_types.
// In similar vein, we call its value a “singular value”.  In other words, a
// singular value is either a scalar or a message value.
//
// This code uses assertions liberally to document and enforce invariants.  In
// optimized builds they should be stripped out.
//
// We make frequent use of the Emacs module API “saturating error” behavior.
// That is, we only check for errors if necessary, e.g., before an expensive
// operation or if we exhibited incorrect observable behavior otherwise.  See
// Info node ‘(elisp) Module Nonlocal’.

// Microsoft Visual C++ mostly supports standard C, but doesn’t define __STDC__
// unless compiling with /Za, which would disable C99 features.  Starting with
// Visual C++ 2015, C99 support is good enough for us.  See
// https://docs.microsoft.com/en-us/cpp/preprocessor/predefined-macros?view=msvc-160#standard-predefined-macros,
// https://docs.microsoft.com/en-us/cpp/build/reference/za-ze-disable-language-extensions?view=msvc-160,
// and
// https://docs.microsoft.com/en-us/cpp/overview/visual-cpp-language-conformance?view=msvc-160#c-standard-library-features-1.
#if !(defined __STDC__ || (defined _MSC_VER && _MSC_VER >= 1900))
#  error this file requires a standards-conformant C compiler
#endif

#if !defined __STDC_VERSION__ || __STDC_VERSION__ < 201112L
#  error this file requires at least C11
#endif

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <wchar.h>

#ifdef _WIN32
#  ifndef UNICODE
#    define UNICODE
#  endif
#  ifndef _UNICODE
#    define _UNICODE
#  endif
#  ifndef STRICT
#    define STRICT
#  endif
#  ifndef NOMINMAX
#    define NOMINMAX
#  endif
#  ifndef WIN32_LEAN_AND_MEAN
#    define WIN32_LEAN_AND_MEAN
#  endif
#  include <windows.h>
static_assert(MAXDWORD <= SIZE_MAX, "unsupported architecture");
#else
#  include <sys/types.h>
#  include <unistd.h>
static_assert(SSIZE_MAX <= SIZE_MAX, "unsupported architecture");
// See Linux and macOS man pages for read(2) and write(2).
enum { kMaxIO = 0x7FFFF000 };
static_assert(kMaxIO <= INT_MAX, "unsupported architecture");
#endif

#ifndef INT32_MAX
#  error this file requires the int32_t type
#endif

#ifndef UINT32_MAX
#  error this file requires the uint32_t type
#endif

#ifndef INT64_MAX
#  error this file requires the int64_t type
#endif

#ifndef UINT64_MAX
#  error this file requires the uint64_t type
#endif

static_assert(INTMAX_MAX <= UINTMAX_MAX, "unsupported architecture");
static_assert(PTRDIFF_MAX <= SIZE_MAX, "unsupported architecture");
static_assert(SIZE_MAX <= UINTMAX_MAX, "unsupported architecture");
static_assert(LLONG_MAX <= INTMAX_MAX && LLONG_MIN >= INTMAX_MIN,
              "unsupported architecture");
static_assert(INT_MAX <= SIZE_MAX, "unsupported architecture");
static_assert(INT_MAX <= PTRDIFF_MAX, "unsupported architecture");

#include "emacs-module.h"

#if !defined EMACS_MAJOR_VERSION || EMACS_MAJOR_VERSION < 29
#  error Emacs module header too old
#endif

#include "absl/base/attributes.h"
#include "absl/base/config.h"
#include "google/protobuf/any.upb.h"
#include "google/protobuf/any.upbdefs.h"
#include "google/protobuf/descriptor.upb.h"
#include "google/protobuf/duration.upb.h"
#include "google/protobuf/duration.upbdefs.h"
#include "google/protobuf/compiler/plugin.upb.h"
#include "google/protobuf/timestamp.upb.h"
#include "google/protobuf/timestamp.upbdefs.h"
#include "upb/base/descriptor_constants.h"
#include "upb/base/status.h"
#include "upb/base/string_view.h"
#include "upb/base/upcast.h"
#include "upb/json/decode.h"
#include "upb/json/encode.h"
#include "upb/mem/arena.h"
#include "upb/message/array.h"
#include "upb/message/map.h"
#include "upb/message/message.h"
#include "upb/reflection/def.h"
#include "upb/reflection/message.h"
#include "upb/text/encode.h"
#include "upb/util/required_fields.h"
#include "upb/wire/decode.h"
#include "upb/wire/encode.h"

/// Global variables

// We maintain a single upb_DefPool object that keeps the definitions of all
// registered protocol buffer descriptors in memory.  The protocol buffer
// compiler (generate.el) writes ‘elisp/proto/register-file-descriptor-set’
// forms into the generated Emacs Lisp files which register the descriptors in
// the pool.  We also keep preallocated interned versions of the symbols that
// this module uses frequently; without that, some of the module functions were
// found to spend up to 30% of their CPU time in calls to ‘intern’.

// All preallocated symbols.  See https://en.wikipedia.org/wiki/X_Macro for the
// pattern.  The first parameter is an enumerator name, the second one the
// symbol as a string literal.
#define GLOBAL_SYMBOLS                                           \
  X(kNil, "nil")                                                 \
  X(kT, "t")                                                     \
  X(kInteger, "integer")                                         \
  X(kFloat, "float")                                             \
  X(kCar, "car")                                                 \
  X(kCdr, "cdr")                                                 \
  X(kIntern, "intern")                                           \
  X(kSymbolName, "symbol-name")                                  \
  X(kSymbolValue, "symbol-value")                                \
  X(kFunctionPut, "function-put")                                \
  X(kNumberp, "numberp")                                         \
  X(kString, "string")                                           \
  X(kStringp, "stringp")                                         \
  X(kMultibyteStringP, "multibyte-string-p")                     \
  X(kUnibyteStringP, "elisp/proto/unibyte-string-p")             \
  X(kBytep, "elisp/proto/bytep")                                 \
  X(kInt32P, "elisp/proto/int32-p")                              \
  X(kUint32P, "elisp/proto/uint32-p")                            \
  X(kInt64P, "elisp/proto/int64-p")                              \
  X(kUint64P, "elisp/proto/uint64-p")                            \
  X(kPlistp, "elisp/proto/plistp")                               \
  X(kCons, "cons")                                               \
  X(kList, "list")                                               \
  X(kNreverse, "nreverse")                                       \
  X(kUnibyteString, "unibyte-string")                            \
  X(kVector, "vector")                                           \
  X(kMakeVector, "make-vector")                                  \
  X(kLength, "length")                                           \
  X(kAref, "aref")                                               \
  X(kInsert, "insert")                                           \
  X(kPrin1, "prin1")                                             \
  X(kPrinc, "princ")                                             \
  X(kPrintLength, "print-length")                                \
  X(kDefalias, "defalias")                                       \
  X(kDefineError, "define-error")                                \
  X(kOverflowError, "overflow-error")                            \
  X(kWrongTypeArgument, "wrong-type-argument")                   \
  X(kArgsOutOfRange, "args-out-of-range")                        \
  X(kFieldNameP, "elisp/proto/field-name-p")                     \
  X(kFieldKeywordP, "elisp/proto/field-keyword-p")               \
  X(kTypeUrlP, "elisp/proto/type-url-p")                         \
  X(kArenaP, "elisp/proto/arena-p")                              \
  X(kMessage, "elisp/proto/message")                             \
  X(kMessageP, "elisp/proto/message-p")                          \
  X(kArray, "elisp/proto/array")                                 \
  X(kArrayP, "elisp/proto/array-p")                              \
  X(kMap, "elisp/proto/map")                                     \
  X(kMapP, "elisp/proto/map-p")                                  \
  X(kMapEntryP, "elisp/proto/map-entry-p")                       \
  X(kSerializedCodeGeneratorRequestP,                            \
    "elisp/proto/serialized-code-generator-request-p")           \
  X(kSerializedFileDescriptorProtoP,                             \
    "elisp/proto/serialized-file-descriptor-proto-p")            \
  X(kObjectArena, "elisp/proto/proto--object-arena")             \
  X(kObjectPtr, "elisp/proto/proto--object-ptr")                 \
  X(kArrayNew, "elisp/proto/proto--array-new")                   \
  X(kMapNew, "elisp/proto/proto--map-new")                       \
  X(kTimestamp, "google/protobuf/Timestamp")                     \
  X(kTimestampP, "google/protobuf/Timestamp-p")                  \
  X(kTimestampNew, "google/protobuf/Timestamp--new")             \
  X(kDuration, "google/protobuf/Duration")                       \
  X(kDurationP, "google/protobuf/Duration-p")                    \
  X(kDurationNew, "google/protobuf/Duration--new")               \
  X(kAny, "google/protobuf/Any")                                 \
  X(kAnyP, "google/protobuf/Any-p")                              \
  X(kSeqLength, "seq-length")                                    \
  X(kSeqDoIndexed, "seq-do-indexed")                             \
  X(kMapDo, "map-do")                                            \
  X(kFunctionType, "function-type")                              \
  X(kSideEffectFree, "side-effect-free")                         \
  X(kFunction, "function")                                       \
  X(kSymbol, "symbol")                                           \
  X(kAndOptional, "&optional")                                   \
  X(kAndRest, "&rest")                                           \
  X(kCAllowPartial, ":allow-partial")                            \
  X(kCDiscardUnknown, ":discard-unknown")                        \
  X(kCDeterministic, ":deterministic")                           \
  X(kCCompact, ":compact")                                       \
  X(kCEmitDefaults, ":emit-defaults")                            \
  X(kCProtoNames, ":proto-names")

// All error symbols.  See https://en.wikipedia.org/wiki/X_Macro for the
// pattern.  The first parameter is an enumerator name, the second one the
// symbol as a string literal without the “elisp/proto/” prefix, the third one
// the error message, and the fourth one the parent symbol.
#define ERROR_SYMBOLS                                                         \
  X(kUnknownMessageType, "unknown-message-type",                              \
    "Unknown protocol buffer message type", kNil)                             \
  X(kUnknownField, "unknown-field", "Unknown protocol buffer message field",  \
    kNil)                                                                     \
  X(kAtomicField, "atomic-field",                                             \
    "Field is not a message, repeated, or map field", kNil)                   \
  X(kNotScalarField, "not-scalar-field", "Field is not a scalar field", kNil) \
  X(kNotMapField, "not-map-field", "Field is not a map field", kNil)          \
  X(kImmutable, "immutable", "Immutable protocol buffer object", kNil)        \
  X(kDuplicateKey, "duplicate-key", "Duplicate keyword argument", kNil)       \
  X(kWrongChoice, "wrong-choice", "Wrong choice of keyword argument", kNil)   \
  X(kParseError, "parse-error",                                               \
    "Error parsing serialized protocol buffer message", kNil)                 \
  X(kSerializeError, "serialize-error",                                       \
    "Error serializing protocol buffer message", kNil)                        \
  X(kJsonParseError, "json-parse-error",                                      \
    "Error parsing protocol buffer message from JSON", kNil)                  \
  X(kJsonSerializeError, "json-serialize-error",                              \
    "Error serializing protocol buffer message to JSON", kNil)                \
  X(kMalformed, "malformed",                                                  \
    "Serialized protocol buffer message is malformed", kParseError)           \
  X(kMalformedUtf8, "malformed-utf-8",                                        \
    "Serialized protocol buffer message contains malformed UTF-8",            \
    kParseError)                                                              \
  X(kUnlinkedSubMessage, "unlinked-sub-message",                              \
    "Internal protocol buffer error", kParseError)                            \
  X(kMissingRequiredField, "missing-required-field",                          \
    "Required protocol buffer field not present", kNil)                       \
  X(kArenaFusionFailed, "arena-fusion-failed",                                \
    "Internal protocol buffer error", kNil)                                   \
  X(kNoPresence, "no-presence",                                               \
    "Protocol buffer field has no notion of presence", kNil)                  \
  X(kUninitializedAny, "uninitialized-any",                                   \
    "Message of type google.protobuf.Any not properly initialized", kNil)     \
  X(kTimestampOverflow, "timestamp-overflow",                                 \
    "Time value not representable as google.protobuf.Timestamp message",      \
    kOverflowError)                                                           \
  X(kDurationOverflow, "duration-overflow",                                   \
    "Time value not representable as google.protobuf.Duration message",       \
    kOverflowError)                                                           \
  X(kFileError, "file-error", "File error", kNil)                             \
  X(kRegistrationFailed, "registration-failed",                               \
    "Could not register file descriptor set", kNil)                           \
  X(kUnknownProtoFile, "unknown-proto-file",                                  \
    "Unknown protocol buffer definition file", kNil)

// clang-format off
// Clang-Format gets confused by this coding structure.
enum GlobalSymbol {
#define X(enumerator, string) enumerator,
  GLOBAL_SYMBOLS
#undef X
#define X(enumerator, string, message, parent) enumerator,
  ERROR_SYMBOLS
#undef X
  kNumSymbols
};
// clang-format on

// Work around what appears to be an IWYU bug.
// IWYU pragma: no_forward_declare GlobalSymbol

struct Cons {
  emacs_value car, cdr;
};

// A singleton structure that holds all global objects.  It is heap-allocated
// and initialized by ‘emacs_module_init’; see Info node ‘(elisp) Module
// Initialization’.  All exported module functions receive the pointer to the
// singleton object as the ‘data’ parameter.
struct Globals {
  upb_DefPool* pool;
  emacs_value symbols[kNumSymbols];
  // Keep the value of ‘memory-signal-data’ preallocated so we can access it in
  // an out-of-memory situation.
  struct Cons memory_signal_data;
};

// Every function that interacts with Emacs in some way receives a Context
// object as its first parameter.  This is merely an abbreviation so that we
// don’t have to pass around the Emacs environments and the pointer to the
// globals object.
struct Context {
  emacs_env* env;
  const struct Globals* globals;
};

static emacs_value GlobalSymbol(struct Context ctx, enum GlobalSymbol sym) {
  assert(sym >= 0 && sym < kNumSymbols);
  return ctx.globals->symbols[sym];
}

static emacs_value Nil(struct Context ctx) { return GlobalSymbol(ctx, kNil); }

static const upb_DefPool* DefPool(struct Context ctx) {
  return ctx.globals->pool;
}

static upb_DefPool* MutableDefPool(struct Context ctx) {
  return ctx.globals->pool;
}

/// Version checks

// Any code that wants to use a newer module function must perform a double
// version check to ensure that both emacs-module.h and the running Emacs binary
// are recent enough, using this pattern:
//
//   #if EMACS_MAJOR_VERSION >= 30
//   if (IsEmacs30(ctx)) {
//     …code that assumes Emacs 30…
//     return …;
//   }
//   #endif
//   …fallback code…

// Uncomment the code below once we actually need a check for Emacs 30:
// #if EMACS_MAJOR_VERSION >= 30
// static bool IsEmacs30(struct Context ctx) {
//   enum { kMinimumSize = sizeof(struct emacs_env_30) };
//   return ctx.env->size >= kMinimumSize;
// }
// #endif

/// Wrappers around module functions

// These functions are simple wrappers around the functions in the Emacs
// environment.  They mostly save typing and cover some common cases.

// Returns whether all Emacs functions since the last call to ClearError have
// succeeded.  See the Info node ‘(elisp) Module Nonlocal’.
static bool Success(struct Context ctx) {
  return ctx.env->non_local_exit_check(ctx.env) == emacs_funcall_exit_return;
}

static void ClearError(struct Context ctx) {
  ctx.env->non_local_exit_clear(ctx.env);
}

static emacs_value MakeBoolean(struct Context ctx, bool value) {
  return GlobalSymbol(ctx, value ? kT : kNil);
}

static bool IsNotNil(struct Context ctx, emacs_value value) {
  return ctx.env->is_not_nil(ctx.env, value);
}

static bool IsNil(struct Context ctx, emacs_value value) {
  return !IsNotNil(ctx, value);
}

static bool Eq(struct Context ctx, emacs_value a, emacs_value b) {
  return ctx.env->eq(ctx.env, a, b);
}

// Checks whether an Emacs value refers to the same object as a
// globally-allocated symbol.
static bool EqGlobal(struct Context ctx, emacs_value a, enum GlobalSymbol b) {
  return Eq(ctx, a, GlobalSymbol(ctx, b));
}

static emacs_value TypeOf(struct Context ctx, emacs_value value) {
  return ctx.env->type_of(ctx.env, value);
}

static emacs_value Funcall(struct Context ctx, emacs_value fun, ptrdiff_t nargs,
                           emacs_value* args) {
  return ctx.env->funcall(ctx.env, fun, nargs, args);
}

static emacs_value Funcall1(struct Context ctx, emacs_value fun,
                            emacs_value arg) {
  return Funcall(ctx, fun, 1, &arg);
}

static emacs_value Funcall2(struct Context ctx, emacs_value fun,
                            emacs_value arg1, emacs_value arg2) {
  emacs_value args[] = {arg1, arg2};
  return Funcall(ctx, fun, 2, args);
}

static emacs_value FuncallSymbol(struct Context ctx, enum GlobalSymbol fun,
                                 ptrdiff_t nargs, emacs_value* args) {
  return Funcall(ctx, GlobalSymbol(ctx, fun), nargs, args);
}

static emacs_value FuncallSymbol1(struct Context ctx, enum GlobalSymbol fun,
                                  emacs_value arg) {
  return FuncallSymbol(ctx, fun, 1, &arg);
}

static emacs_value FuncallSymbol2(struct Context ctx, enum GlobalSymbol fun,
                                  emacs_value arg1, emacs_value arg2) {
  emacs_value args[] = {arg1, arg2};
  return FuncallSymbol(ctx, fun, 2, args);
}

static emacs_value FuncallSymbol3(struct Context ctx, enum GlobalSymbol fun,
                                  emacs_value arg1, emacs_value arg2,
                                  emacs_value arg3) {
  emacs_value args[] = {arg1, arg2, arg3};
  return FuncallSymbol(ctx, fun, 3, args);
}

static bool Predicate(struct Context ctx, enum GlobalSymbol predicate,
                      emacs_value value) {
  return IsNotNil(ctx, FuncallSymbol1(ctx, predicate, value));
}

static emacs_value List(struct Context ctx, ptrdiff_t nargs,
                        emacs_value* args) {
  return nargs == 0 ? Nil(ctx) : FuncallSymbol(ctx, kList, nargs, args);
}

static emacs_value List2(struct Context ctx, emacs_value arg1,
                         emacs_value arg2) {
  emacs_value args[] = {arg1, arg2};
  return List(ctx, 2, args);
}

static emacs_value List3(struct Context ctx, emacs_value arg1, emacs_value arg2,
                         emacs_value arg3) {
  emacs_value args[] = {arg1, arg2, arg3};
  return List(ctx, 3, args);
}

static emacs_value List5(struct Context ctx, emacs_value arg1, emacs_value arg2,
                         emacs_value arg3, emacs_value arg4, emacs_value arg5) {
  emacs_value args[] = {arg1, arg2, arg3, arg4, arg5};
  return List(ctx, 5, args);
}

static void Signal(struct Context ctx, enum GlobalSymbol symbol,
                   ptrdiff_t nargs, emacs_value* args) {
  emacs_value data = List(ctx, nargs, args);
  ctx.env->non_local_exit_signal(ctx.env, GlobalSymbol(ctx, symbol), data);
}

static void Signal0(struct Context ctx, enum GlobalSymbol symbol) {
  ctx.env->non_local_exit_signal(ctx.env, GlobalSymbol(ctx, symbol), Nil(ctx));
}

static void Signal1(struct Context ctx, enum GlobalSymbol symbol,
                    emacs_value arg) {
  Signal(ctx, symbol, 1, &arg);
}

static void Signal2(struct Context ctx, enum GlobalSymbol symbol,
                    emacs_value arg1, emacs_value arg2) {
  emacs_value args[] = {arg1, arg2};
  Signal(ctx, symbol, 2, args);
}

static void Signal3(struct Context ctx, enum GlobalSymbol symbol,
                    emacs_value arg1, emacs_value arg2, emacs_value arg3) {
  emacs_value args[] = {arg1, arg2, arg3};
  Signal(ctx, symbol, 3, args);
}

static void MemoryFull(struct Context ctx) {
  struct Cons data = ctx.globals->memory_signal_data;
  ctx.env->non_local_exit_signal(ctx.env, data.car, data.cdr);
}

static void OverflowError0(struct Context ctx) { Signal0(ctx, kOverflowError); }

static void OverflowError1(struct Context ctx, emacs_value arg) {
  Signal1(ctx, kOverflowError, arg);
}

// Returns whether a + b would overflow.  If not, set *r to a + b.  Otherwise,
// the value of *r is unspecified.
ABSL_MUST_USE_RESULT static bool AddOverflowSize(struct Context ctx, size_t a,
                                                 size_t b, size_t* r) {
  bool overflow;
#if ABSL_HAVE_BUILTIN(__builtin_add_overflow)
  overflow = __builtin_add_overflow(a, b, r);
#else
  size_t c = a + b;
  overflow = c < a;
  if (!overflow) *r = c;
#endif
  if (overflow) OverflowError0(ctx);
  return overflow;
}

// Returns whether a * b would overflow.  If not, set *r to a * b.  Otherwise,
// the value of *r is unspecified.
ABSL_MUST_USE_RESULT static bool MultiplyOverflowSize(struct Context ctx,
                                                      size_t a, size_t b,
                                                      size_t* r) {
  bool overflow;
#if ABSL_HAVE_BUILTIN(__builtin_mul_overflow)
  overflow = __builtin_mul_overflow(a, b, r);
#else
  size_t c = a * b;
  overflow = a != 0 && c / a != b;
  if (!overflow) *r = c;
#endif
  if (overflow) OverflowError0(ctx);
  return overflow;
}

static void WrongTypeArgument(struct Context ctx, enum GlobalSymbol predicate,
                              emacs_value value) {
  Signal2(ctx, kWrongTypeArgument, GlobalSymbol(ctx, predicate), value);
}

static bool CheckType(struct Context ctx, enum GlobalSymbol predicate,
                      emacs_value value) {
  bool ok = Predicate(ctx, predicate, value);
  if (!ok) WrongTypeArgument(ctx, predicate, value);
  return ok;
}

static void ArgsOutOfRange2(struct Context ctx, emacs_value sequence,
                            emacs_value index) {
  Signal2(ctx, kArgsOutOfRange, sequence, index);
}

static void ArgsOutOfRange3(struct Context ctx, emacs_value sequence,
                            emacs_value from, emacs_value to) {
  Signal3(ctx, kArgsOutOfRange, sequence, from, to);
}

// Signals an error indicating that the given keyword argument is duplicated in
// an argument list.
static void DuplicateKey(struct Context ctx, emacs_value key) {
  Signal1(ctx, kDuplicateKey, key);
}

// Signals an error indicating that the given keyword argument is unknown.
static void WrongChoice(struct Context ctx, emacs_value arg, ptrdiff_t nchoices,
                        emacs_value* choices) {
  Signal2(ctx, kWrongChoice, arg, List(ctx, nchoices, choices));
}

typedef emacs_value (*Function)(emacs_env*, ptrdiff_t, emacs_value*, void*);

static emacs_value MakeFunction(struct Context ctx, ptrdiff_t min_arity,
                                ptrdiff_t max_arity, Function fun,
                                const char* doc, void* data) {
  assert(min_arity >= 0);
  assert(max_arity >= min_arity || max_arity == emacs_variadic_function);
  return ctx.env->make_function(ctx.env, min_arity, max_arity, fun, doc, data);
}

static emacs_value MakeInteger(struct Context ctx, intmax_t value) {
  return ctx.env->make_integer(ctx.env, value);
}

static intmax_t ExtractInteger(struct Context ctx, emacs_value value) {
  return ctx.env->extract_integer(ctx.env, value);
}

// Extracts a signed integer that should be within the range of a given C type.
// Signals a ‘wrong-type-argument’ error with the given predicate on failure;
// the predicate is not otherwise used.
static intmax_t ExtractTypedInteger(struct Context ctx,
                                    enum GlobalSymbol predicate, intmax_t min,
                                    intmax_t max, emacs_value value) {
  intmax_t i = ExtractInteger(ctx, value);
  if (i < min || i > max) WrongTypeArgument(ctx, predicate, value);
  return i;
}

enum {
  kLimbBits = sizeof(emacs_limb_t) * CHAR_BIT,
  kLimbsForUintmax =
      (sizeof(uintmax_t) + sizeof(emacs_limb_t) - 1) / sizeof(emacs_limb_t)
};

// Protocol buffers and the µpb library use unsigned types heavily, so we have
// some helper functions to convert them as well.

static emacs_value MakeUInteger(struct Context ctx, uintmax_t value) {
  if (value <= INTMAX_MAX) return MakeInteger(ctx, (intmax_t)value);
  emacs_limb_t limbs[kLimbsForUintmax];
#if EMACS_LIMB_MAX >= UINTMAX_MAX
  static_assert(kLimbsForUintmax == 1, "kLimbsForUintmax != 1");
  limbs[0] = value;
#else
  for (size_t i = 0; i < kLimbsForUintmax; ++i) {
    limbs[i] = value;
    value >>= kLimbBits;
  }
#endif
  return ctx.env->make_big_integer(ctx.env, +1, kLimbsForUintmax, limbs);
}

// Extracts an unsigned integer that should be within the range of a given
// C type.  Signals a ‘wrong-type-argument’ error with the given predicate on
// failure; the predicate is not otherwise used.
static uintmax_t ExtractTypedUInteger(struct Context ctx,
                                      enum GlobalSymbol predicate,
                                      uintmax_t max, emacs_value value) {
  // No need to go through big integer codepath.
  if (max <= INTMAX_MAX) {
    return (uintmax_t)ExtractTypedInteger(ctx, predicate, 0, (intmax_t)max,
                                          value);
  }
  // Short-circuit if we’re already failing so that the ClearError below
  // doesn’t clear unrelated errors.
  if (!Success(ctx)) return 0;
  int sign;
  ptrdiff_t count = kLimbsForUintmax;
  emacs_limb_t limbs[kLimbsForUintmax];
  bool ok = ctx.env->extract_big_integer(ctx.env, value, &sign, &count, limbs);
  if (!ok || sign < 0 || count > kLimbsForUintmax) {
    // Don’t leak internal out-of-range signal to the user.
    ClearError(ctx);
    WrongTypeArgument(ctx, predicate, value);
    return 0;
  }
  if (sign == 0) return 0;
  assert(count > 0);
  uintmax_t u = 0;
  for (size_t i = 0; i < (size_t)count; ++i) {
    u |= (limbs[i] << (i * kLimbBits));
  }
  if (u > max) WrongTypeArgument(ctx, predicate, value);
  return u;
}

static emacs_value MakeFloat(struct Context ctx, double value) {
  return ctx.env->make_float(ctx.env, value);
}

static double ExtractFloat(struct Context ctx, emacs_value value) {
  return ctx.env->extract_float(ctx.env, value);
}

static double ExtractNumber(struct Context ctx, emacs_value value) {
  emacs_value type = TypeOf(ctx, value);
  if (EqGlobal(ctx, type, kInteger)) return (double)ExtractInteger(ctx, value);
  if (EqGlobal(ctx, type, kFloat)) return ExtractFloat(ctx, value);
  WrongTypeArgument(ctx, kNumberp, value);
  return 0;
}

static emacs_value MakeString(struct Context ctx, upb_StringView value) {
  if (value.size > PTRDIFF_MAX) {
    OverflowError1(ctx, MakeUInteger(ctx, value.size));
    return NULL;
  }
  return ctx.env->make_string(ctx.env, value.data, (ptrdiff_t)value.size);
}

struct Allocator {
  void* (*alloc)(size_t size, void* data);
  void (*free)(void* ptr, void* data);
  void* data;
};

static void* HeapAllocatorAlloc(size_t size, void* data ABSL_ATTRIBUTE_UNUSED) {
  return malloc(size);
}

static void HeapAllocatorFree(void* ptr, void* data ABSL_ATTRIBUTE_UNUSED) {
  free(ptr);
}

static struct Allocator HeapAllocator(void) {
  struct Allocator ret = {HeapAllocatorAlloc, HeapAllocatorFree, NULL};
  return ret;
}

static void* ArenaAllocatorAlloc(size_t size, void* data) {
  return upb_Arena_Malloc(data, size);
}

static void ArenaAllocatorFree(void* ptr ABSL_ATTRIBUTE_UNUSED,
                               void* data ABSL_ATTRIBUTE_UNUSED) {}

static struct Allocator ArenaAllocator(upb_Arena* arena) {
  struct Allocator ret = {ArenaAllocatorAlloc, ArenaAllocatorFree, arena};
  return ret;
}

static void* Allocate(struct Context ctx, struct Allocator alloc, size_t size) {
  void* ptr = alloc.alloc(size, alloc.data);
  if (ptr == NULL && size > 0) MemoryFull(ctx);
  return ptr;
}

static void* AllocateArray(struct Context ctx, struct Allocator alloc,
                           size_t count, size_t size) {
  assert(size > 0);
  if (MultiplyOverflowSize(ctx, count, size, &size)) return NULL;
  return Allocate(ctx, alloc, size);
}

static emacs_value* AllocateLispArray(struct Context ctx,
                                      struct Allocator alloc, size_t count) {
  return AllocateArray(ctx, alloc, count, sizeof(emacs_value));
}

static void* AllocateFromArena(struct Context ctx, upb_Arena* arena,
                               size_t size) {
  void* ptr = upb_Arena_Malloc(arena, size);
  if (ptr == NULL) MemoryFull(ctx);
  return ptr;
}

static void Free(struct Allocator alloc, void* ptr) {
  alloc.free(ptr, alloc.data);
}

// A string with a given length.  This is just like upb_StringView, except that
// the string is mutable.  Functions that return a MutableString should
// null-terminate it so that s.data[s.size] is valid and zero.  If a function
// allocates a MutableString, the caller is responsible for deallocating it.
struct MutableString {
  char* data;
  size_t size;
};

static struct MutableString ExtractString(struct Context ctx,
                                          struct Allocator alloc,
                                          emacs_value value) {
  struct MutableString null = {NULL, 0};
  ptrdiff_t size;
  if (!ctx.env->copy_string_contents(ctx.env, value, NULL, &size)) return null;
  assert(size > 0);
  char* data = Allocate(ctx, alloc, (size_t)size);
  if (data == NULL) return null;
  if (!ctx.env->copy_string_contents(ctx.env, value, data, &size)) {
    Free(alloc, data);
    return null;
  }
  // Exclude trailing null byte.
  assert(size > 0);
  struct MutableString ret = {data, (size_t)(size - 1)};
  return ret;
}

// Serialized protocol buffer messages are represented as unibyte strings, so we
// have some helper functions to convert them.

static emacs_value MakeUnibyteString(struct Context ctx, upb_StringView value) {
  if (value.size > PTRDIFF_MAX) {
    OverflowError1(ctx, MakeUInteger(ctx, value.size));
    return NULL;
  }
  return ctx.env->make_unibyte_string(ctx.env, value.data,
                                      (ptrdiff_t)value.size);
}

static struct MutableString ExtractUnibyteString(struct Context ctx,
                                                 struct Allocator alloc,
                                                 emacs_value value) {
  struct MutableString null = {NULL, 0};
  if (!Predicate(ctx, kStringp, value) ||
      Predicate(ctx, kMultibyteStringP, value)) {
    WrongTypeArgument(ctx, kUnibyteStringP, value);
    return null;
  }
  emacs_value lisp_length = FuncallSymbol1(ctx, kLength, value);
  intmax_t length = ExtractInteger(ctx, lisp_length);
  if (length < 0 || (uintmax_t)length >= SIZE_MAX) {
    OverflowError1(ctx, lisp_length);
    return null;
  }
  assert((uintmax_t)length < SIZE_MAX);
  size_t size = (size_t)length + 1;
  assert(size > 0 && size > (uintmax_t)length);
  char* data = Allocate(ctx, alloc, size);
  if (data == NULL) return null;
  for (intmax_t i = 0; i < length; ++i) {
    data[i] = (char)(unsigned char)ExtractTypedUInteger(
        ctx, kBytep, UCHAR_MAX,
        FuncallSymbol2(ctx, kAref, value, MakeInteger(ctx, i)));
  }
  data[length] = '\0';
  struct MutableString ret = {data, (size_t)length};
  return ret;
}

static emacs_value MakeUserPtr(struct Context ctx, void* ptr,
                               void (*fin)(void*)) {
  return ctx.env->make_user_ptr(ctx.env, fin, ptr);
}

static void* ExtractUserPtr(struct Context ctx, emacs_value value) {
  return ctx.env->get_user_ptr(ctx.env, value);
}

static emacs_value Cons(struct Context ctx, emacs_value car, emacs_value cdr) {
  return FuncallSymbol2(ctx, kCons, car, cdr);
}

static emacs_value Car(struct Context ctx, emacs_value list) {
  return FuncallSymbol1(ctx, kCar, list);
}

static emacs_value Cdr(struct Context ctx, emacs_value list) {
  return FuncallSymbol1(ctx, kCdr, list);
}

static void Push(struct Context ctx, emacs_value newelt, emacs_value* list) {
  *list = Cons(ctx, newelt, *list);
}

static emacs_value Pop(struct Context ctx, emacs_value* list) {
  emacs_value car = Car(ctx, *list);
  *list = Cdr(ctx, *list);
  return car;
}

// The time conversion functions assume that time_t is integral and fits into
// intmax_t.  This should always be the case on supported systems, but can’t be
// verified easily.

// Converts a timespec structure to a Lisp timestamp.  The timespec value need
// not be canonical, i.e., its tv_sec and tv_nsec fields can have arbitrary
// values.  If successful, the return value will be a valid Lisp timestamp as
// defined in the Info node ‘(elisp) Time of Day’.
static emacs_value MakeTime(struct Context ctx, struct timespec value) {
  // make_time accepts non-canonical time representations, see Info node
  // ‘(elisp) Module Values’.
  return ctx.env->make_time(ctx.env, value);
}

// Converts a Lisp time value to a timespec structure.  The Lisp value can be an
// arbitrary time value.  See Info node ‘(elisp) Time of Day’ for the valid time
// values.  If successful, the return value will be in canonical form, i.e.,
// tv_nsec will be in the closed interval [0, 999999999].
static struct timespec ExtractTime(struct Context ctx, emacs_value value) {
  return ctx.env->extract_time(ctx.env, value);
}

static emacs_value MakeVector(struct Context ctx, ptrdiff_t size) {
  return FuncallSymbol2(ctx, kMakeVector, MakeInteger(ctx, size), Nil(ctx));
}

static void SetVectorElement(struct Context ctx, emacs_value vector,
                             ptrdiff_t i, emacs_value value) {
  ctx.env->vec_set(ctx.env, vector, i, value);
}

static struct MutableString SymbolName(struct Context ctx,
                                       struct Allocator alloc,
                                       emacs_value symbol) {
  return ExtractString(ctx, alloc, FuncallSymbol1(ctx, kSymbolName, symbol));
}

static emacs_value SymbolValue(struct Context ctx, enum GlobalSymbol symbol) {
  return FuncallSymbol1(ctx, kSymbolValue, GlobalSymbol(ctx, symbol));
}

#ifndef NDEBUG
static bool IsAscii(const char* string) {
  for (const char* p = string; *p != '\0'; ++p) {
    if ((unsigned char)*p > 0x7F) return false;
  }
  return true;
}
#endif

static emacs_value InternAscii(struct Context ctx, const char* name) {
  assert(IsAscii(name));
  return ctx.env->intern(ctx.env, name);
}

static emacs_value Intern(struct Context ctx, upb_StringView name) {
  return FuncallSymbol1(ctx, kIntern, MakeString(ctx, name));
}

ABSL_MUST_USE_RESULT static emacs_value Nreverse(struct Context ctx,
                                                 emacs_value value) {
  return FuncallSymbol1(ctx, kNreverse, value);
}

static void Prin1(struct Context ctx, emacs_value object, emacs_value stream) {
  FuncallSymbol2(ctx, kPrin1, object, stream);
}

static void Princ(struct Context ctx, emacs_value object, emacs_value stream) {
  FuncallSymbol2(ctx, kPrinc, object, stream);
}

static void PrincString(struct Context ctx, upb_StringView string,
                        emacs_value stream) {
  Princ(ctx, MakeString(ctx, string), stream);
}

static void PrincChar(struct Context ctx, char ch, emacs_value stream) {
  upb_StringView string = UPB_STRINGVIEW_INIT(&ch, 1);
  PrincString(ctx, string, stream);
}

static void PrincLiteral(struct Context ctx, const char* string,
                         emacs_value stream) {
  PrincString(ctx, upb_StringView_FromString(string), stream);
}

// Returns the value of the ‘print-length’ variable.  Returns 0 if that is
// negative and SIZE_MAX if it’s nil (i.e., no limit).
static size_t PrintLength(struct Context ctx) {
  emacs_value o = SymbolValue(ctx, kPrintLength);
  if (IsNil(ctx, o)) return SIZE_MAX;
  intmax_t n = ExtractInteger(ctx, o);
  if (n < 0) return 0;
  if ((uintmax_t)n > (uintmax_t)SIZE_MAX) return SIZE_MAX;  // saturate
  return (size_t)n;
}

/// String manipulation

static upb_StringView View(struct MutableString string) {
  return upb_StringView_FromDataAndSize(string.data, string.size);
}

// Safely concatenates multiple strings into a heap-allocated buffer.  When
// successful, the returned string pointer is non-NULL and points to a
// null-terminated string.
static struct MutableString Concat(struct Context ctx, struct Allocator alloc,
                                   ptrdiff_t nargs,
                                   const upb_StringView* args) {
  struct MutableString null = {NULL, 0};
  size_t length = 0;
  for (ptrdiff_t i = 0; i < nargs; ++i) {
    bool overflow = AddOverflowSize(ctx, length, args[i].size, &length);
    if (overflow) return null;
  }
  size_t size;
  bool overflow = AddOverflowSize(ctx, length, 1, &size);
  if (overflow) return null;
  assert(size > 0);
  char* buffer = Allocate(ctx, alloc, size);
  if (buffer == NULL) return null;
  char* ptr = buffer;
  for (ptrdiff_t i = 0; i < nargs; ++i) {
    upb_StringView arg = args[i];
    memcpy(ptr, arg.data, arg.size);
    ptr += arg.size;
  }
  assert(ptr == buffer + length);
  *ptr = '\0';
  struct MutableString ret = {buffer, length};
  return ret;
}

static struct MutableString Concat2(struct Context ctx, struct Allocator alloc,
                                    upb_StringView a, upb_StringView b) {
  upb_StringView args[] = {a, b};
  return Concat(ctx, alloc, 2, args);
}

static void ReplaceChar(struct MutableString string, char from, char to) {
  for (size_t i = 0; i < string.size; ++i) {
    if (string.data[i] == from) string.data[i] = to;
  }
}

/// Arenas

// See
// https://github.com/protocolbuffers/protobuf/blob/main/docs/upb/wrapping-upb.md
// and
// https://github.com/protocolbuffers/protobuf/blob/main/docs/upb/vs-cpp-protos.md
// for some discussion around arenas and how they should be treated in
// language-specific wrappers.

// An arena with the associated user pointer.  The ‘lisp’ member must refer to a
// user pointer that points to the same arena as ‘ptr’.  When garbage-collecting
// the Lisp object, the arena will be freed.  This implements the unique
// ownership semantics described in the section “Integrating GC with µpb” in
// https://github.com/protocolbuffers/protobuf/blob/main/docs/upb/wrapping-upb.md.
// All objects allocated from the arena must be wrapped in Lisp structures that
// point to the Lisp representation of the arena; this implements the required
// shared ownership semantics.  The MakeArena and ExtractArena functions return
// a LispArena structure with the right constraints.
struct LispArena {
  upb_Arena* ptr;
  emacs_value lisp;
};

static upb_Arena* NewArena(struct Context ctx) {
  upb_Arena* arena = upb_Arena_New();
  if (arena == NULL) MemoryFull(ctx);
  return arena;
}

static void FreeArena(void* ptr) { upb_Arena_Free(ptr); }

// Allocate and return a new arena and a user pointer pointing to it.
static struct LispArena MakeArena(struct Context ctx) {
  struct LispArena null = {NULL, NULL};
  upb_Arena* ptr = NewArena(ctx);
  if (ptr == NULL) return null;
  emacs_value lisp = MakeUserPtr(ctx, ptr, FreeArena);
  if (!Success(ctx)) {
    upb_Arena_Free(ptr);
    return null;
  }
  struct LispArena arena = {ptr, lisp};
  return arena;
}

// Assumes that the given value holds a user pointer that points to an upb_Arena
// object.  Returns that pointer and the value itself.
static struct LispArena ExtractArena(struct Context ctx, emacs_value value) {
  struct LispArena null = {NULL, NULL};
  upb_Arena* ptr = ExtractUserPtr(ctx, value);
  if (ptr == NULL || !Success(ctx)) {
    WrongTypeArgument(ctx, kArenaP, value);
    return null;
  }
  struct LispArena arena = {ptr, value};
  return arena;
}

// Fuses the given arenas, and signals an error if that failed.  The two
// arguments may refer to the same arena, in which case the function is a no-op.
ABSL_MUST_USE_RESULT static bool FuseArenas(struct Context ctx, upb_Arena* a,
                                            upb_Arena* b) {
  bool ok = upb_Arena_Fuse(a, b);
  if (!ok) Signal0(ctx, kArenaFusionFailed);
  return ok;
}

/// Conversion from and to Lisp structures

// Arena-allocated objects (messages, arrays, maps) are represented in Lisp as
// subtypes of the structure type ‘elisp/proto/object’, which is defined in
// proto.el.  ‘elisp/proto/object’ holds two fields, a reference to an arena
// user pointer and the user pointer representing the object itself.
//
// These objects come in two flavors, mutable and immutable.  Functions like
// ‘elisp/proto/field’ return immutable objects; other functions like
// ‘elisp/proto/mutable-field’ return mutable objects.  These correspond to
// pointers to ‘const’ and non-’const’ objects in C, respectively.  The
// mutability bit is stored in the object itself.

// A C representation of a structure of type ‘elisp/proto/object’.  The ‘ptr’
// field points to the actual object; it must be allocated from the given arena.
struct LispStructure {
  struct LispArena arena;
  const void* ptr;
};

// Creates a new object of a type that’s a subtype of ‘elisp/proto/object’.  The
// constructor must refer to a constructor function that creates and initializes
// the object; the constructor function must take two arguments, an arena user
// pointer and the user pointer for the object itself.
static emacs_value MakeStructure(struct Context ctx, emacs_value constructor,
                                 struct LispArena arena, void* ptr) {
  return Funcall2(ctx, constructor, arena.lisp, MakeUserPtr(ctx, ptr, NULL));
}

// Verifies that the given Lisp value refers to an object of a subtype of
// ‘elisp/proto/object’.  Uses the given predicate to check for the object type.
static struct LispStructure ExtractStructure(struct Context ctx,
                                             enum GlobalSymbol predicate,
                                             emacs_value value) {
  struct LispStructure null = {{NULL, NULL}, NULL};
  if (!CheckType(ctx, predicate, value)) return null;
  struct LispArena arena =
      ExtractArena(ctx, FuncallSymbol1(ctx, kObjectArena, value));
  if (arena.ptr == NULL) return null;
  const void* ptr = ExtractUserPtr(ctx, FuncallSymbol1(ctx, kObjectPtr, value));
  if (ptr == NULL || !Success(ctx)) {
    WrongTypeArgument(ctx, predicate, value);
    return null;
  }
  struct LispStructure ret = {arena, ptr};
  return ret;
}

// Signals an error of type ‘elisp/proto/immutable’.
static void Immutable(struct Context ctx, emacs_value value) {
  Signal1(ctx, kImmutable, value);
}

/// Messages

// User pointer representing a protocol buffer message.  Since upb_Message
// objects are untyped, we also store the type information alongside the value.
// The ‘mutable’ field determines whether the message is mutable; if not,
// callers shouldn’t modify the message.  Callers will typically use
// ExtractMessage or ExtractMutableMessage to obtain upb_Message objects from
// Lisp arguments.
struct TypedMessage {
  const upb_MessageDef* type;
  upb_Message* value;
  bool mutable;
};

// A C representation of the ‘elisp/proto/message’ structure type.  Callers
// should typically use MessageArg and MutableMessageArg instead.
struct LispMessage {
  struct LispArena arena;
  const struct TypedMessage* ptr;
};

// A C representation of a mutable or immutable ‘elisp/proto/message’ structure
// object.  Module functions should typically use ExtractMessage to obtain a
// MessageArg structure from a Lisp value.
struct MessageArg {
  struct LispArena arena;
  const upb_MessageDef* type;
  const upb_Message* value;
};

// A C representation of a mutable ‘elisp/proto/message’ structure object.
// Module functions should typically use ExtractMutableMessage to obtain a
// MutableMessageArg structure from a Lisp value.
struct MutableMessageArg {
  struct LispArena arena;
  const upb_MessageDef* type;
  upb_Message* value;
};

// Returns a Lisp string value containing the full name of the given message
// type.  This is typically used for error messages.
static emacs_value MakeMessageName(struct Context ctx,
                                   const upb_MessageDef* def) {
  upb_StringView full_name =
      upb_StringView_FromString(upb_MessageDef_FullName(def));
  return MakeString(ctx, full_name);
}

// Returns a Lisp string value containing the full name of the given field.
// This is typically used for error messages.
static emacs_value MakeFieldName(struct Context ctx, const upb_FieldDef* def) {
  upb_StringView full_name =
      upb_StringView_FromString(upb_FieldDef_FullName(def));
  return MakeString(ctx, full_name);
}

// Returns a Lisp list containing the names of the fields of the given message
// type as symbols.  This is typically used for error messages.
static emacs_value MakeMessageFields(struct Context ctx,
                                     const upb_MessageDef* def) {
  struct Allocator alloc = HeapAllocator();
  int count = upb_MessageDef_FieldCount(def);
  assert(count >= 0);
  if (count == 0) return Nil(ctx);
  emacs_value* names = AllocateLispArray(ctx, alloc, (size_t)count);
  if (names == NULL) return NULL;
  for (int i = 0; i < count; ++i) {
    const upb_FieldDef* field = upb_MessageDef_Field(def, i);
    upb_StringView name = upb_StringView_FromString(upb_FieldDef_Name(field));
    names[i] = Intern(ctx, name);
  }
  emacs_value ret = List(ctx, count, names);
  Free(alloc, names);
  return ret;
}

// Returns the name of a special function for a given message type.  Constructs
// the name by concatenating the Lisp structure name with the given suffix.
// These functions need to be generated for each message type by the protocol
// buffer compiler in generate.el.
static struct MutableString MessageFunctionName(struct Context ctx,
                                                struct Allocator alloc,
                                                const char* suffix,
                                                const upb_MessageDef* def) {
  upb_StringView full_name =
      upb_StringView_FromString(upb_MessageDef_FullName(def));
  struct MutableString ret =
      Concat2(ctx, alloc, full_name, upb_StringView_FromString(suffix));
  // This transformation must match the code in generate.el.
  assert(strchr(suffix, '.') == NULL);
  ReplaceChar(ret, '.', '/');
  return ret;
}

// Returns the name of the type predicate for the given message type.
static struct MutableString MessagePredicateName(struct Context ctx,
                                                 struct Allocator alloc,
                                                 const upb_MessageDef* def) {
  return MessageFunctionName(ctx, alloc, "-p", def);
}

// Returns the name of the internal constructor for the given message type.  The
// constructor must be called with two arguments, a user pointer pointing to an
// upb_Arena, and a user pointer pointing to a TypedMessage.
static struct MutableString MessageConstructorName(struct Context ctx,
                                                   struct Allocator alloc,
                                                   const upb_MessageDef* def) {
  return MessageFunctionName(ctx, alloc, "--new", def);
}

// Signals an error of type ‘wrong-type-argument’ indicating that the given Lisp
// value is not of a specific message type.
static void WrongMessageType(struct Context ctx, const upb_MessageDef* def,
                             emacs_value value) {
  struct Allocator alloc = HeapAllocator();
  struct MutableString predicate = MessagePredicateName(ctx, alloc, def);
  if (predicate.data == NULL) return;
  Signal2(ctx, kWrongTypeArgument, Intern(ctx, View(predicate)), value);
  Free(alloc, predicate.data);
}

// Creates a new Lisp structure object of the right type for the given message
// definition.  Uses the constructor function to create the Lisp structure
// object.  If ‘mutable’ is ‘false’, then callers can pass in a ‘const
// upb_Message*’ cast to a ‘upb_Message*’; otherwise they need to ensure that
// the message is indeed mutable.  This is a rather low-level function; callers
// should typically use MakeMessage or MakeMutableMessage instead.
static emacs_value MakeMessageStruct(struct Context ctx, struct LispArena arena,
                                     emacs_value constructor,
                                     const upb_MessageDef* def,
                                     upb_Message* value, bool mutable) {
  struct TypedMessage msg = {def, value, mutable};
  struct TypedMessage* ptr = AllocateFromArena(ctx, arena.ptr, sizeof *ptr);
  if (ptr == NULL) return NULL;
  *ptr = msg;
  return MakeStructure(ctx, constructor, arena, ptr);
}

// Returns a C representation of a Lisp message structure.  Callers should check
// whether the message is mutable before attempting to mutate it.  This is a
// rather low-level function; callers should typically use ExtractMessage or
// ExtractMutableMessage instead.
static struct LispMessage ExtractMessageStruct(struct Context ctx,
                                               emacs_value value) {
  struct LispMessage null = {{NULL, NULL}, NULL};
  struct LispStructure str = ExtractStructure(ctx, kMessageP, value);
  const struct TypedMessage* ptr = str.ptr;
  if (ptr == NULL) return null;
  struct LispMessage ret = {str.arena, ptr};
  return ret;
}

// Creates a new Lisp structure object of the right type for the given message
// definition.  Derives the constructor function from the message definition.
// If ‘mutable’ is ‘false’, then callers can pass in a ‘const upb_Message*’ cast
// to a ‘upb_Message*’; otherwise they need to ensure that the message is indeed
// mutable.  This is a rather low-level function; callers should typically use
// MakeMessage or MakeMutableMessage instead.
static emacs_value ConstructMessage(struct Context ctx, struct LispArena arena,
                                    const upb_MessageDef* def,
                                    upb_Message* value, bool mutable) {
  struct Allocator alloc = HeapAllocator();
  struct MutableString constructor = MessageConstructorName(ctx, alloc, def);
  if (constructor.data == NULL) return NULL;
  emacs_value lisp = MakeMessageStruct(
      ctx, arena, Intern(ctx, View(constructor)), def, value, mutable);
  Free(alloc, constructor.data);
  return lisp;
}

// Creates the Lisp representation for an immutable message.
static emacs_value MakeMessage(struct Context ctx, struct LispArena arena,
                               const upb_MessageDef* def,
                               const upb_Message* value) {
  return ConstructMessage(ctx, arena, def, (upb_Message*)value, false);
}

// Returns an immutable message and its type from its Lisp representation.  This
// also works if the underlying message is mutable.  Module functions should use
// this function to extract messages from Lisp function arguments.
static struct MessageArg ExtractMessage(struct Context ctx, emacs_value value) {
  struct MessageArg null = {{NULL, NULL}, NULL, NULL};
  struct LispMessage msg = ExtractMessageStruct(ctx, value);
  if (msg.ptr == NULL) return null;
  struct MessageArg ret = {msg.arena, msg.ptr->type, msg.ptr->value};
  return ret;
}

// Creates the Lisp representation for a mutable message.
static emacs_value MakeMutableMessage(struct Context ctx,
                                      struct LispArena arena,
                                      const upb_MessageDef* def,
                                      upb_Message* value) {
  return ConstructMessage(ctx, arena, def, value, true);
}

// Returns a mutable message and its type from its Lisp representation.  Signals
// an error if the underlying message is immutable.  Module functions should use
// this function to extract mutable messages from Lisp function arguments.
static struct MutableMessageArg ExtractMutableMessage(struct Context ctx,
                                                      emacs_value value) {
  struct MutableMessageArg null = {{NULL, NULL}, NULL, NULL};
  struct LispStructure str = ExtractStructure(ctx, kMessageP, value);
  const struct TypedMessage* ptr = str.ptr;
  if (ptr == NULL) return null;
  if (!ptr->mutable) {
    Immutable(ctx, value);
    return null;
  }
  struct MutableMessageArg ret = {str.arena, ptr->type, ptr->value};
  return ret;
}

// Returns an immutable message of a well-known type from its Lisp
// representation.  This also works if the underlying message is mutable.
// Module functions should use this function to extract messages of well-known
// types from Lisp function arguments.
static struct MessageArg ExtractWellKnownMessage(struct Context ctx,
                                                 upb_WellKnown type,
                                                 enum GlobalSymbol predicate,
                                                 emacs_value value) {
  struct MessageArg null = {{NULL, NULL}, NULL, NULL};
  struct MessageArg msg = ExtractMessage(ctx, value);
  if (msg.type == NULL) return null;
  if (upb_MessageDef_WellKnownType(msg.type) != type) {
    WrongTypeArgument(ctx, predicate, value);
    return null;
  }
  return msg;
}

// Returns a mutable message of a well-known type from its Lisp representation.
// Signals an error if the underlying message is immutable.  Module functions
// should use this function to extract mutable messages of well-known types from
// Lisp function arguments.
static struct MutableMessageArg ExtractWellKnownMutableMessage(
    struct Context ctx, upb_WellKnown type, enum GlobalSymbol predicate,
    emacs_value value) {
  struct MutableMessageArg null = {{NULL, NULL}, NULL, NULL};
  struct MutableMessageArg msg = ExtractMutableMessage(ctx, value);
  if (msg.type == NULL) return null;
  if (upb_MessageDef_WellKnownType(msg.type) != type) {
    WrongTypeArgument(ctx, predicate, value);
    return null;
  }
  return msg;
}

// A C representation of a mutable or immutable ‘elisp/proto/message’ structure
// object representing a google.protobuf.Any message.  Module functions should
// typically use ExtractAny to obtain an AnyArg structure from a Lisp value.
struct AnyArg {
  struct LispArena arena;
  const google_protobuf_Any* value;
};

// Returns an immutable message of the type google.protobuf.Any from its Lisp
// representation.  This also works if the underlying message is mutable.
// Module functions should use this function to extract google.protobuf.Any
// messages from Lisp function arguments.
static struct AnyArg ExtractAny(struct Context ctx, emacs_value value) {
  struct AnyArg null = {{NULL, NULL}, NULL};
  struct MessageArg msg =
      ExtractWellKnownMessage(ctx, kUpb_WellKnown_Any, kAnyP, value);
  if (msg.type == NULL) return null;
  union {
    const upb_Message* msg;
    const google_protobuf_Any* any;
  } u = {msg.value};
  struct AnyArg any = {msg.arena, u.any};
  return any;
}

// Allocates a new message from the given arena.  Signals an error if memory is
// full.
static upb_Message* NewMessage(struct Context ctx, upb_Arena* arena,
                               const upb_MessageDef* def) {
  upb_Message* msg = upb_Message_New(upb_MessageDef_MiniTable(def), arena);
  if (msg == NULL) MemoryFull(ctx);
  return msg;
}

// Signals an error using the given protocol buffer status.
static void ProtoError(struct Context ctx, enum GlobalSymbol symbol,
                       const upb_Status* status) {
  upb_StringView message =
      upb_StringView_FromString(upb_Status_ErrorMessage(status));
  Signal1(ctx, symbol, MakeString(ctx, message));
}

// Signals an error indicating that the given message type is not registered.
static void UnknownMessageType(struct Context ctx, upb_StringView name) {
  Signal1(ctx, kUnknownMessageType, MakeString(ctx, name));
}

// Signals an error indicating that the given message field is not one of the
// fields in the message definition.
static void UnknownField(struct Context ctx, const upb_MessageDef* def,
                         upb_StringView field) {
  Signal3(ctx, kUnknownField, MakeMessageName(ctx, def), Intern(ctx, field),
          MakeMessageFields(ctx, def));
}

// Signals an error indicating that the given field is atomic.
static void AtomicField(struct Context ctx, const upb_FieldDef* def) {
  Signal1(ctx, kAtomicField, MakeFieldName(ctx, def));
}

// Signals an error indicating that the given message field is not a scalar
// field.
static void NotScalarField(struct Context ctx, const upb_FieldDef* def) {
  Signal1(ctx, kNotScalarField, MakeFieldName(ctx, def));
}

// Signals an error indicating that the given message field is not a map field.
static void NotMapField(struct Context ctx, const upb_FieldDef* def) {
  Signal1(ctx, kNotMapField, MakeFieldName(ctx, def));
}

// Signals an error indicating that parsing a serialized message has failed.
static void ParseError(struct Context ctx, upb_DecodeStatus status,
                       emacs_value value) {
  switch (status) {
    case kUpb_DecodeStatus_Ok:
      break;
    case kUpb_DecodeStatus_Malformed:
      Signal1(ctx, kMalformed, value);
      break;
    case kUpb_DecodeStatus_OutOfMemory:
      MemoryFull(ctx);
      break;
    case kUpb_DecodeStatus_BadUtf8:
      Signal1(ctx, kMalformedUtf8, value);
      break;
    case kUpb_DecodeStatus_MaxDepthExceeded:
      OverflowError1(ctx, value);
      break;
    case kUpb_DecodeStatus_MissingRequired:
      Signal1(ctx, kMissingRequiredField, value);
      break;
    case kUpb_DecodeStatus_UnlinkedSubMessage:
      Signal1(ctx, kUnlinkedSubMessage, value);
      break;
    default:
      Signal2(ctx, kParseError, MakeInteger(ctx, status), value);
      break;
  }
}

// Signals an error indicating that serializing a message has failed.
static void SerializeError(struct Context ctx, upb_EncodeStatus status,
                           const upb_MessageDef* def) {
  switch (status) {
    case kUpb_EncodeStatus_Ok:
      break;
    case kUpb_EncodeStatus_OutOfMemory:
      MemoryFull(ctx);
      break;
    case kUpb_EncodeStatus_MaxDepthExceeded:
      OverflowError1(ctx, MakeMessageName(ctx, def));
      break;
    case kUpb_EncodeStatus_MissingRequired:
      Signal1(ctx, kMissingRequiredField, MakeMessageName(ctx, def));
      break;
    default:
      Signal2(ctx, kSerializeError, MakeInteger(ctx, status),
              MakeMessageName(ctx, def));
      break;
  }
}

// Signals an error indicating that a field has no notion of presence.
static void NoPresence(struct Context ctx, const upb_FieldDef* def) {
  Signal1(ctx, kNoPresence, MakeFieldName(ctx, def));
}

// Signals an error indicating that a google.protobuf.Any message isn’t properly
// initialized.
static void UninitializedAny(struct Context ctx, emacs_value value) {
  Signal1(ctx, kUninitializedAny, value);
}

// Returns the serialized form of the given message.  Wraps upb_Encode.
static struct MutableString SerializeMessage(struct Context ctx,
                                             upb_Arena* arena,
                                             const upb_MessageDef* def,
                                             const upb_Message* msg,
                                             int options) {
  struct MutableString null = {NULL, 0};
  char* data;
  size_t size;
  upb_EncodeStatus status = upb_Encode(msg, upb_MessageDef_MiniTable(def),
                                       options, arena, &data, &size);
  if (status != kUpb_EncodeStatus_Ok) {
    SerializeError(ctx, status, def);
    return null;
  }
  struct MutableString ret = {data, size};
  return ret;
}

// Returns the textual form of the given message.  Wraps upb_TextEncode.
static struct MutableString SerializeMessageText(struct Context ctx,
                                                 struct Allocator alloc,
                                                 const upb_MessageDef* def,
                                                 const upb_Message* msg,
                                                 int options) {
  struct MutableString null = {NULL, 0};
  // First, try with a small buffer.
  size_t size = 0x1000;
  char* buffer = Allocate(ctx, alloc, size);
  if (buffer == NULL) return null;
  size_t length = upb_TextEncode(msg, def, DefPool(ctx), options, buffer, size);
  if (length < size) {
    struct MutableString ret = {buffer, length};
    return ret;
  }
  // Otherwise we need to allocate a larger buffer.
  Free(alloc, buffer);
  bool overflow = AddOverflowSize(ctx, length, 1, &size);
  if (overflow) return null;
  assert(size > 0);
  buffer = Allocate(ctx, alloc, size);
  if (buffer == NULL) return null;
  length = upb_TextEncode(msg, def, DefPool(ctx), options, buffer, size);
  assert(length == size - 1);
  struct MutableString ret = {buffer, length};
  return ret;
}

// Returns the JSON form of the given message.  Wraps upb_JsonEncode.
static struct MutableString SerializeMessageJson(struct Context ctx,
                                                 struct Allocator alloc,
                                                 const upb_MessageDef* def,
                                                 const upb_Message* msg,
                                                 int options) {
  struct MutableString null = {NULL, 0};
  // First, try with a small buffer.
  size_t size = 0x1000;
  char* buffer = Allocate(ctx, alloc, size);
  if (buffer == NULL) return null;
  upb_Status status;
  upb_Status_Clear(&status);
  size_t length =
      upb_JsonEncode(msg, def, DefPool(ctx), options, buffer, size, &status);
  if (upb_Status_IsOk(&status) && length < size) {
    struct MutableString ret = {buffer, length};
    return ret;
  }
  // Otherwise we need to allocate a larger buffer.
  Free(alloc, buffer);
  bool overflow = AddOverflowSize(ctx, length, 1, &size);
  if (overflow) return null;
  assert(size > 0);
  buffer = Allocate(ctx, alloc, size);
  if (buffer == NULL) return null;
  upb_Status_Clear(&status);
  length =
      upb_JsonEncode(msg, def, DefPool(ctx), options, buffer, size, &status);
  if (!upb_Status_IsOk(&status)) {
    Free(alloc, buffer);
    ProtoError(ctx, kJsonSerializeError, &status);
    return null;
  }
  assert(length == size - 1);
  struct MutableString ret = {buffer, length};
  return ret;
}

// Parses a message from its serialized form.  The serialized form must be
// allocated from the same arena as the message.  This wraps upb_Decode.
static upb_Message* ParseMessage(struct Context ctx, upb_Arena* arena,
                                 const upb_MessageDef* def,
                                 upb_StringView serialized, int options) {
  upb_Message* msg = NewMessage(ctx, arena, def);
  if (msg == NULL) return NULL;
  // We can always have strings alias the input buffer because the serialized
  // string and the message are allocated from the same arena.
  upb_DecodeStatus status = upb_Decode(
      serialized.data, serialized.size, msg, upb_MessageDef_MiniTable(def),
      upb_DefPool_ExtensionRegistry(DefPool(ctx)),
      options | kUpb_DecodeOption_AliasString, arena);
  if (status != kUpb_DecodeStatus_Ok) {
    ParseError(ctx, status, MakeUnibyteString(ctx, serialized));
    return NULL;
  }
  return msg;
}

// Parses a message from its JSON form.  This wraps upb_JsonDecode.
static upb_Message* ParseMessageJson(struct Context ctx, upb_Arena* arena,
                                     const upb_MessageDef* def,
                                     upb_StringView json, int options) {
  upb_Message* msg = NewMessage(ctx, arena, def);
  if (msg == NULL) return NULL;
  upb_Status status;
  upb_Status_Clear(&status);
  if (!upb_JsonDecode(json.data, json.size, msg, def, DefPool(ctx), options,
                      arena, &status)) {
    ProtoError(ctx, kJsonParseError, &status);
    return NULL;
  }
  return msg;
}

// Prints the fields of the given message without quoting characters or
// delimiters, similar to ‘princ’.
static void PrincFields(struct Context ctx, const upb_MessageDef* def,
                        const upb_Message* msg, emacs_value stream) {
  struct Allocator alloc = HeapAllocator();
  struct MutableString string =
      SerializeMessageText(ctx, alloc, def, msg, UPB_TXTENC_SINGLELINE);
  if (string.data == NULL) return;
  PrincString(ctx, View(string), stream);
  Free(alloc, string.data);
}

/// Type-erased scalar and singular values

// This section deals with converting scalar and singular values (i.e., values
// that are neither arrays nor maps) from and to Lisp.  We specify their types
// using upb_FieldDef, ignoring the repeated or map status of the field.  This
// is because µpb has no notion of value types and doesn’t allow us to “strip
// off” the repeated or map status from a field definition.

// Attempts to convert a scalar value from a compatible Lisp object to the given
// type.  If the type refers to a repeated field, the function acts as if it
// referred to a corresponding non-repeated field.  The conversion is loose,
// e.g., integers can be converted to floating-point numbers.
static upb_MessageValue AdoptScalar(struct Context ctx, upb_Arena* arena,
                                    const upb_FieldDef* type,
                                    emacs_value value) {
  upb_MessageValue dest;
  switch (upb_FieldDef_CType(type)) {
    case kUpb_CType_Bool:
      dest.bool_val = IsNotNil(ctx, value);
      break;
    case kUpb_CType_Float:
      dest.float_val = (float)ExtractNumber(ctx, value);
      break;
    case kUpb_CType_Int32:
    case kUpb_CType_Enum:
      dest.int32_val = (int32_t)ExtractTypedInteger(ctx, kInt32P, INT32_MIN,
                                                    INT32_MAX, value);
      break;
    case kUpb_CType_UInt32:
      dest.uint32_val =
          (uint32_t)ExtractTypedUInteger(ctx, kUint32P, UINT32_MAX, value);
      break;
    case kUpb_CType_Double:
      dest.double_val = ExtractNumber(ctx, value);
      break;
    case kUpb_CType_Int64:
      dest.int64_val = (int64_t)ExtractTypedInteger(ctx, kInt64P, INT64_MIN,
                                                    INT64_MAX, value);
      break;
    case kUpb_CType_UInt64:
      dest.uint64_val =
          (uint64_t)ExtractTypedUInteger(ctx, kUint64P, UINT64_MAX, value);
      break;
    case kUpb_CType_String:
      // We have to allocate strings from the correct arena and may not free
      // them here to avoid dangling pointers.
      dest.str_val = View(ExtractString(ctx, ArenaAllocator(arena), value));
      break;
    case kUpb_CType_Bytes:
      dest.str_val =
          View(ExtractUnibyteString(ctx, ArenaAllocator(arena), value));
      break;
    default:
      NotScalarField(ctx, type);
      dest = upb_MessageValue_Zero();
      break;
  }
  return dest;
}

// Attempts to convert a singular value from a compatible Lisp object to the
// given type.  If the type refers to a repeated field, the function acts as if
// it referred to a corresponding non-repeated field.  The conversion is loose,
// e.g., integers can be converted to floating-point numbers, but message types
// must match exactly.
static upb_MessageValue AdoptSingular(struct Context ctx, upb_Arena* arena,
                                      const upb_FieldDef* type,
                                      emacs_value value) {
  if (upb_FieldDef_IsSubMessage(type)) {
    upb_MessageValue dest;
    dest.msg_val = NULL;
    struct MessageArg msg = ExtractMessage(ctx, value);
    if (msg.type == NULL) return dest;
    const upb_MessageDef* def = upb_FieldDef_MessageSubDef(type);
    if (msg.type != def) {
      WrongMessageType(ctx, def, value);
      return dest;
    }
    // The message in ‘value’ might live in another arena, so we must fuse the
    // arenas here.
    if (!FuseArenas(ctx, arena, msg.arena.ptr)) return dest;
    dest.msg_val = msg.value;
    return dest;
  }
  return AdoptScalar(ctx, arena, type, value);
}

// Creates a Lisp object from a scalar value of the given type.  If the type
// refers to a repeated field, the function acts as if it referred to a
// corresponding non-repeated field.
static emacs_value MakeScalar(struct Context ctx, const upb_FieldDef* type,
                              upb_MessageValue value) {
  switch (upb_FieldDef_CType(type)) {
    case kUpb_CType_Bool:
      return MakeBoolean(ctx, value.bool_val);
    case kUpb_CType_Float:
      return MakeFloat(ctx, value.float_val);
    case kUpb_CType_Int32:
    case kUpb_CType_Enum:
      return MakeInteger(ctx, value.int32_val);
    case kUpb_CType_UInt32:
      return MakeUInteger(ctx, value.uint32_val);
    case kUpb_CType_Double:
      return MakeFloat(ctx, value.double_val);
    case kUpb_CType_Int64:
      return MakeInteger(ctx, value.int64_val);
    case kUpb_CType_UInt64:
      return MakeUInteger(ctx, value.uint64_val);
    case kUpb_CType_String:
      return MakeString(ctx, value.str_val);
    case kUpb_CType_Bytes:
      return MakeUnibyteString(ctx, value.str_val);
    default:
      NotScalarField(ctx, type);
      return NULL;
  }
}

// Creates a Lisp object from a singular value of the given type.  If the type
// refers to a repeated field, the function acts as if it referred to a
// corresponding non-repeated field.
static emacs_value MakeSingular(struct Context ctx, struct LispArena arena,
                                const upb_FieldDef* type,
                                upb_MessageValue value) {
  if (upb_FieldDef_IsSubMessage(type)) {
    return MakeMessage(ctx, arena, upb_FieldDef_MessageSubDef(type),
                       value.msg_val);
  }
  return MakeScalar(ctx, type, value);
}

// Prints a scalar value to the given stream using ‘prin1’.
static void Prin1Scalar(struct Context ctx, const upb_FieldDef* type,
                        upb_MessageValue value, emacs_value stream) {
  Prin1(ctx, MakeScalar(ctx, type, value), stream);
}

// Prints a singular value to the given stream similar to ‘prin1’.  Message
// values are not quoted.
static void Prin1Singular(struct Context ctx, const upb_FieldDef* type,
                          upb_MessageValue value, emacs_value stream) {
  if (upb_FieldDef_IsSubMessage(type)) {
    PrincLiteral(ctx, "{ ", stream);
    PrincFields(ctx, upb_FieldDef_MessageSubDef(type), value.msg_val, stream);
    PrincChar(ctx, '}', stream);
  } else {
    Prin1Scalar(ctx, type, value, stream);
  }
}

/// Arrays

// We use a dedicated array data type to represent repeated message fields.
// Code in proto.el ensures that arrays are generalized sequences in the
// ‘seq.el’ sense.  Arrays can be mutable or immutable.

// User pointer representing an array.  Since upb_Array objects are untyped, we
// also store the type information alongside the value.  Since there are no
// dedicated array definitions in µpb, we use the definition of the
// corresponding repeated message field instead; it must refer to a repeated
// non-map message field.  The ‘mutable’ field determines whether the array is
// mutable; if not, callers shouldn’t modify the array.  Callers will typically
// use ExtractArray or ExtractMutableArray to obtain upb_Array objects from Lisp
// arguments.
struct TypedArray {
  const upb_FieldDef* type;
  upb_Array* value;
  bool mutable;
};

// A C representation of the ‘elisp/proto/array’ structure type.  Callers should
// typically use ArrayArg and MutableArrayArg instead.
struct LispArray {
  struct LispArena arena;
  const struct TypedArray* ptr;
};

// A C representation of a mutable or immutable ‘elisp/proto/array’ structure
// object.  Module functions should typically use ExtractArray to obtain an
// ArrayArg structure from a Lisp value.
struct ArrayArg {
  struct LispArena arena;
  const upb_FieldDef* type;
  const upb_Array* value;
};

// A C representation of a mutable ‘elisp/proto/array’ structure object.  Module
// functions should typically use ExtractMutableArray to obtain a
// MutableArrayArg structure from a Lisp value.
struct MutableArrayArg {
  struct LispArena arena;
  const upb_FieldDef* type;
  upb_Array* value;
};

// Creates a new Lisp array structure object of the right type for the given
// repeated field definition.  If ‘mutable’ is ‘false’, then callers can pass in
// a ‘const upb_Array*’ cast to a ‘upb_Array*’; otherwise they need to ensure
// that the array is indeed mutable.  This is a rather low-level function;
// callers should typically use MakeArray or MakeMutableArray instead.
static emacs_value MakeArrayStruct(struct Context ctx, struct LispArena arena,
                                   const upb_FieldDef* type, upb_Array* value,
                                   bool mutable) {
  assert(upb_FieldDef_IsRepeated(type));
  assert(!upb_FieldDef_IsMap(type));
  assert(value != NULL);
  struct TypedArray array = {type, value, mutable};
  struct TypedArray* ptr = AllocateFromArena(ctx, arena.ptr, sizeof *ptr);
  if (ptr == NULL) return NULL;
  *ptr = array;
  return MakeStructure(ctx, GlobalSymbol(ctx, kArrayNew), arena, ptr);
}

// Returns a C representation of a Lisp array structure.  Callers should check
// whether the array is mutable before attempting to mutate it.  This is a
// rather low-level function; callers should typically use ExtractArray or
// ExtractMutableArray instead.
static struct LispArray ExtractArrayStruct(struct Context ctx,
                                           emacs_value value) {
  struct LispArray null = {{NULL, NULL}, NULL};
  struct LispStructure str = ExtractStructure(ctx, kArrayP, value);
  const struct TypedArray* ptr = str.ptr;
  if (ptr == NULL) return null;
  struct LispArray ret = {str.arena, ptr};
  return ret;
}

// Creates the Lisp representation for an immutable array.
static emacs_value MakeArray(struct Context ctx, struct LispArena arena,
                             const upb_FieldDef* type, const upb_Array* value) {
  return MakeArrayStruct(ctx, arena, type, (upb_Array*)value, false);
}

// Returns an immutable array and its type from its Lisp representation.  This
// also works if the underlying array is mutable.  Module functions should use
// this function to extract arrays from Lisp function arguments.
static struct ArrayArg ExtractArray(struct Context ctx, emacs_value value) {
  struct ArrayArg null = {{NULL, NULL}, NULL, NULL};
  struct LispArray array = ExtractArrayStruct(ctx, value);
  if (array.ptr == NULL) return null;
  struct ArrayArg ret = {array.arena, array.ptr->type, array.ptr->value};
  return ret;
}

// Creates the Lisp representation for a mutable array.
static emacs_value MakeMutableArray(struct Context ctx, struct LispArena arena,
                                    const upb_FieldDef* type,
                                    upb_Array* value) {
  return MakeArrayStruct(ctx, arena, type, value, true);
}

// Returns a mutable array and its type from its Lisp representation.  Signals
// an error if the underlying array is immutable.  Module functions should use
// this function to extract mutable arrays from Lisp function arguments.
static struct MutableArrayArg ExtractMutableArray(struct Context ctx,
                                                  emacs_value value) {
  struct MutableArrayArg null = {{NULL, NULL}, NULL, NULL};
  struct LispStructure str = ExtractStructure(ctx, kArrayP, value);
  const struct TypedArray* ptr = str.ptr;
  if (ptr == NULL) return null;
  if (!ptr->mutable) {
    Immutable(ctx, value);
    return null;
  }
  struct MutableArrayArg ret = {str.arena, ptr->type, ptr->value};
  return ret;
}

// Resizes the given array.  Initializes new elements to 0/NULL; callers should
// typically set their values to actual objects.
static bool ResizeArray(struct Context ctx, upb_Arena* arena, upb_Array* array,
                        size_t length) {
  bool ok = upb_Array_Resize(array, length, arena);
  if (!ok) MemoryFull(ctx);
  return ok;
}

// Allocates a new array from the given arena and resizes it to the given size.
// Signals an error if memory is full.
static upb_Array* NewArray(struct Context ctx, upb_Arena* arena,
                           const upb_FieldDef* type, size_t length) {
  assert(upb_FieldDef_IsRepeated(type));
  upb_Array* array = upb_Array_New(arena, upb_FieldDef_CType(type));
  if (array == NULL) {
    MemoryFull(ctx);
    return NULL;
  }
  if (!ResizeArray(ctx, arena, array, length)) return NULL;
  return array;
}

// Parse an index argument for a sequence of the given size.  The sequence
// itself is only used for error reporting and can be nil.
static size_t ExtractIndex(struct Context ctx, const upb_Array* array,
                           emacs_value sequence, emacs_value index) {
  size_t size = upb_Array_Size(array);
  if (size > INTMAX_MAX) {
    OverflowError1(ctx, MakeUInteger(ctx, size));
    return 0;
  }
  intmax_t ssize = (intmax_t)size;
  intmax_t i = ExtractInteger(ctx, index);
  if (i < 0 || i >= ssize) {
    ArgsOutOfRange2(ctx, sequence, index);
    return 0;
  }
  assert((uintmax_t)i <= SIZE_MAX);
  return (size_t)i;
}

struct SetArrayElementContext {
  const struct Globals* globals;
  upb_Arena* arena;
  const upb_FieldDef* type;
  upb_Array* array;
};

// Helper function for AdoptSequence.  The data pointer must point to a
// SetArrayElementContext structure.  The function is called with two arguments,
// the array element and its index within the array.
static emacs_value SetArrayElement(emacs_env* env,
                                   ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                                   emacs_value* args, void* data) {
  const struct SetArrayElementContext* child_ctx = data;
  struct Context ctx = {env, child_ctx->globals};
  assert(nargs == 2);
  upb_Array* array = child_ctx->array;
  upb_MessageValue value =
      AdoptSingular(ctx, child_ctx->arena, child_ctx->type, args[0]);
  size_t index = ExtractIndex(ctx, array, Nil(ctx), args[1]);
  if (!Success(ctx)) return NULL;
  upb_Array_Set(array, index, value);
  return Nil(ctx);
}

// Attempts to convert a repeated field value from a compatible Lisp object to
// the given type.  The type must refer to a repeated non-map field.  The
// conversion accepts array objects and generalized sequences.
static const upb_Array* AdoptSequence(struct Context ctx, upb_Arena* arena,
                                      const upb_FieldDef* type,
                                      emacs_value value) {
  assert(upb_FieldDef_IsRepeated(type));
  assert(!upb_FieldDef_IsMap(type));
  if (Predicate(ctx, kArrayP, value)) {
    // Fast path for an existing array of the right type.
    struct ArrayArg array = ExtractArray(ctx, value);
    if (array.type == NULL) return NULL;
    if (array.type == type) {
      // The array in ‘value’ might live in another arena, so we must fuse the
      // arenas here.
      if (!FuseArenas(ctx, arena, array.arena.ptr)) return NULL;
      // The types are the same, so we can reuse the value as is.
      return array.value;
    }
    // The types are not the same, so we have to convert the values.  We could
    // special-case this here, but the generic code below works just fine for
    // this case.
  }
  // Use generalized sequence functions to convert from an arbitrary sequence.
  emacs_value lisp_length = FuncallSymbol1(ctx, kSeqLength, value);
  intmax_t length = ExtractInteger(ctx, lisp_length);
  if (length < 0 || (uintmax_t)length > SIZE_MAX || !Success(ctx)) {
    OverflowError1(ctx, lisp_length);
    return NULL;
  }
  upb_Array* array = NewArray(ctx, arena, type, (size_t)length);
  if (array == NULL) return NULL;
  // Call ‘seq-do-indexed’.  For some sequence types (e.g., lists), this might
  // be more efficient than random access.
  struct SetArrayElementContext child_ctx = {ctx.globals, arena, type, array};
  emacs_value fun = MakeFunction(ctx, 2, 2, SetArrayElement, NULL, &child_ctx);
  FuncallSymbol2(ctx, kSeqDoIndexed, fun, value);
  return Success(ctx) ? array : NULL;
}

struct SortContext {
  struct Context base;
  emacs_value pred;
  const upb_FieldDef* array_type;
  const upb_Array* array;
};

struct ArrayElement {
  const struct SortContext* ctx;
  upb_MessageValue val;
  emacs_value lisp;
};

// Helper function for SortArray.  The two pointers must point to ArrayElement
// objects.
static int CompareArrayElements(const void* p, const void* q) {
  const struct ArrayElement* u = p;
  const struct ArrayElement* v = q;
  assert(u->ctx == v->ctx);
  const struct SortContext* sort_ctx = v->ctx;
  struct Context ctx = sort_ctx->base;
  emacs_value pred = sort_ctx->pred;
  emacs_value a = u->lisp;
  emacs_value b = v->lisp;
  bool less = IsNotNil(ctx, Funcall2(ctx, pred, a, b));
  if (less) return -1;
  bool greater = IsNotNil(ctx, Funcall2(ctx, pred, b, a));
  if (greater) return +1;
  return 0;
}

struct RangeArg {
  bool ok;
  size_t from, to;
};

// Parses a range argument pair following the conventions of ‘substring’ or
// ‘seq-subseq’.  The first argument is only used for error reporting and can be
// nil.
static struct RangeArg ExtractRange(struct Context ctx, const upb_Array* array,
                                    ptrdiff_t nargs, emacs_value* args) {
  struct RangeArg null = {false, 0, 0};
  size_t size = upb_Array_Size(array);
  if (size > INTMAX_MAX) {
    OverflowError1(ctx, MakeUInteger(ctx, size));
    return null;
  }
  intmax_t ssize = (intmax_t)size;
  assert(nargs == 2 || nargs == 3);
  emacs_value to_arg = nargs == 3 ? args[2] : Nil(ctx);
  intmax_t from = ExtractInteger(ctx, args[1]);
  if (from < 0) from += ssize;
  intmax_t to = IsNil(ctx, to_arg) ? ssize : ExtractInteger(ctx, to_arg);
  if (to < 0) to += ssize;
  if (from < 0 || from > ssize || to < 0 || to > ssize || to < from ||
      !Success(ctx)) {
    ArgsOutOfRange3(ctx, args[0], args[1], to_arg);
    return null;
  }
  assert((uintmax_t)from <= SIZE_MAX);
  assert((uintmax_t)to <= SIZE_MAX);
  struct RangeArg ret = {true, (size_t)from, (size_t)to};
  return ret;
}

/// Maps

// We use a dedicated map data type to represent map message fields.  Code in
// proto.el ensures that arrays are generalized maps in the ‘map.el’ sense.
// Maps can be mutable or immutable.
//
// In the protocol buffer serialization format, map fields are repeated fields
// of a special map entry message type.

struct MapType {
  const upb_FieldDef* key;
  const upb_FieldDef* value;
};

// Extracts key and value type from a map field definition.  Signals and error
// if the field is not a map field.
static struct MapType GetMapType(struct Context ctx,
                                 const upb_FieldDef* field) {
  struct MapType null = {NULL, NULL};
  if (!upb_FieldDef_IsMap(field)) {
    NotMapField(ctx, field);
    return null;
  }
  const upb_MessageDef* def = upb_FieldDef_MessageSubDef(field);
  const upb_FieldDef* key =
      upb_MessageDef_FindFieldByNumber(def, kUpb_MapEntry_KeyFieldNumber);
  const upb_FieldDef* value =
      upb_MessageDef_FindFieldByNumber(def, kUpb_MapEntry_ValueFieldNumber);
  // Check for some forbidden key or value types.  See
  // https://developers.google.com/protocol-buffers/docs/proto3#maps for the
  // restrictions.  These should normally not be possible because the protocol
  // buffer compiler should already reject them, but we check here again just to
  // be on the safe side.
  if (!upb_MessageDef_IsMapEntry(def) || key == NULL || value == NULL ||
      !(upb_FieldDef_IsPrimitive(key) || upb_FieldDef_IsString(key)) ||
      upb_FieldDef_IsRepeated(value) || upb_FieldDef_IsMap(value)) {
    WrongTypeArgument(ctx, kMapEntryP, MakeMessageName(ctx, def));
    return null;
  }
  struct MapType type = {key, value};
  return type;
}

// User pointer representing a map.  Since upb_Map objects are untyped, we also
// store the type information alongside the value.  Since there are no dedicated
// map definitions in µpb, we use the definition of the corresponding map
// message field instead.  The ‘mutable’ field determines whether the map is
// mutable; if not, callers shouldn’t modify the map.  Callers will typically
// use ExtractMap or ExtractMutableMap to obtain upb_Map objects from Lisp
// arguments.
struct TypedMap {
  const upb_FieldDef* type;
  upb_Map* value;
  bool mutable;
};

// A C representation of the ‘elisp/proto/map’ structure type.  Callers should
// typically use MapArg and MutableMapArg instead.
struct LispMap {
  struct LispArena arena;
  const struct TypedMap* ptr;
};

// A C representation of a mutable or immutable ‘elisp/proto/map’ structure
// object.  Module functions should typically use ExtractMap to obtain a MapArg
// structure from a Lisp value.
struct MapArg {
  struct LispArena arena;
  const upb_FieldDef* type;
  const upb_Map* value;
};

// A C representation of a mutable ‘elisp/proto/map’ structure object.  Module
// functions should typically use ExtractMutableMap to obtain a MutableMapArg
// structure from a Lisp value.
struct MutableMapArg {
  struct LispArena arena;
  const upb_FieldDef* type;
  upb_Map* value;
};

// Creates a new Lisp map structure object of the right type for the given map
// field definition.  If ‘mutable’ is ‘false’, then callers can pass in a ‘const
// upb_Map*’ cast to a ‘upb_Map*’; otherwise they need to ensure that the map is
// indeed mutable.  This is a rather low-level function; callers should
// typically use MakeMap or MakeMutableMap instead.
static emacs_value MakeMapStruct(struct Context ctx, struct LispArena arena,
                                 const upb_FieldDef* type, upb_Map* value,
                                 bool mutable) {
  assert(upb_FieldDef_IsMap(type));
  assert(value != NULL);
  struct TypedMap map = {type, value, mutable};
  struct TypedMap* ptr = AllocateFromArena(ctx, arena.ptr, sizeof *ptr);
  if (ptr == NULL) return NULL;
  *ptr = map;
  return MakeStructure(ctx, GlobalSymbol(ctx, kMapNew), arena, ptr);
}

// Returns a C representation of a Lisp map structure.  Callers should check
// whether the map is mutable before attempting to mutate it.  This is a rather
// low-level function; callers should typically use ExtractMap or
// ExtractMutableMap instead.
static struct LispMap ExtractMapStruct(struct Context ctx, emacs_value value) {
  struct LispMap null = {{NULL, NULL}, NULL};
  struct LispStructure str = ExtractStructure(ctx, kMapP, value);
  const struct TypedMap* ptr = str.ptr;
  if (ptr == NULL) return null;
  struct LispMap ret = {str.arena, ptr};
  return ret;
}

// Creates the Lisp representation for an immutable map.
static emacs_value MakeMap(struct Context ctx, struct LispArena arena,
                           const upb_FieldDef* type, const upb_Map* value) {
  return MakeMapStruct(ctx, arena, type, (upb_Map*)value, false);
}

// Returns an immutable map and its type from its Lisp representation.  This
// also works if the underlying map is mutable.  Module functions should use
// this function to extract maps from Lisp function arguments.
static struct MapArg ExtractMap(struct Context ctx, emacs_value value) {
  struct MapArg null = {{NULL, NULL}, NULL, NULL};
  struct LispMap map = ExtractMapStruct(ctx, value);
  if (map.ptr == NULL) return null;
  struct MapArg ret = {map.arena, map.ptr->type, map.ptr->value};
  return ret;
}

// Creates the Lisp representation for a mutable map.
static emacs_value MakeMutableMap(struct Context ctx, struct LispArena arena,
                                  const upb_FieldDef* type, upb_Map* value) {
  return MakeMapStruct(ctx, arena, type, value, true);
}

// Returns a mutable map and its type from its Lisp representation.  Signals an
// error if the underlying map is immutable.  Module functions should use this
// function to extract mutable maps from Lisp function arguments.
static struct MutableMapArg ExtractMutableMap(struct Context ctx,
                                              emacs_value value) {
  struct MutableMapArg null = {{NULL, NULL}, NULL, NULL};
  struct LispStructure str = ExtractStructure(ctx, kMapP, value);
  const struct TypedMap* ptr = str.ptr;
  if (ptr == NULL) return null;
  if (!ptr->mutable) {
    Immutable(ctx, value);
    return null;
  }
  struct MutableMapArg ret = {str.arena, ptr->type, ptr->value};
  return ret;
}

// Allocates a new map from the given arena.  Signals an error if memory is
// full.
static upb_Map* NewMap(struct Context ctx, upb_Arena* arena,
                       struct MapType type) {
  upb_Map* map = upb_Map_New(arena, upb_FieldDef_CType(type.key),
                             upb_FieldDef_CType(type.value));
  if (map == NULL) MemoryFull(ctx);
  return map;
}

// Inserts or updates a map entry.  Returns whether the operation succeeded.
static upb_MapInsertStatus SetMapEntry(struct Context ctx, upb_Arena* arena,
                                       upb_Map* map, upb_MessageValue key,
                                       upb_MessageValue value) {
  upb_MapInsertStatus status = upb_Map_Insert(map, key, value, arena);
  if (status == kUpb_MapInsertStatus_OutOfMemory) MemoryFull(ctx);
  return status;
}

struct PutMapElementContext {
  const struct Globals* globals;
  upb_Arena* arena;
  struct MapType type;
  upb_Map* map;
};

// Helper function for AdoptMap.  The data pointer must point to a
// PutMapElementContext structure.  The function is called with two arguments,
// the key and the value.  If the key is already present, it is overwritten.
static emacs_value PutMapElement(emacs_env* env,
                                 ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                                 emacs_value* args, void* data) {
  const struct PutMapElementContext* child_ctx = data;
  struct Context ctx = {env, child_ctx->globals};
  upb_Arena* arena = child_ctx->arena;
  struct MapType type = child_ctx->type;
  upb_Map* map = child_ctx->map;
  assert(nargs == 2);
  emacs_value key_arg = args[0];
  upb_MessageValue key = AdoptScalar(ctx, arena, type.key, key_arg);
  upb_MessageValue value = AdoptSingular(ctx, arena, type.value, args[1]);
  if (!Success(ctx)) return NULL;
  SetMapEntry(ctx, arena, map, key, value);
  return Nil(ctx);
}

// Attempts to convert a map field value from a compatible Lisp object to the
// given type.  The type must refer to a map field.  The conversion accepts
// protocol buffer map objects and generalized maps.
static const upb_Map* AdoptMap(struct Context ctx, upb_Arena* arena,
                               const upb_FieldDef* field, emacs_value value) {
  assert(upb_FieldDef_IsMap(field));
  if (Predicate(ctx, kMapP, value)) {
    // Fast path for an existing map of the right type.
    struct MapArg map = ExtractMap(ctx, value);
    if (map.type == NULL) return NULL;
    if (map.type == field) {
      // The map in ‘value’ might live in another arena, so we must fuse the
      // arenas here.
      if (!FuseArenas(ctx, arena, map.arena.ptr)) return NULL;
      // The types are the same, so we can reuse the value as is.
      return map.value;
    }
    // The types are not the same, so we have to convert the entries.  We could
    // special-case this here, but the generic code below works just fine for
    // this case.
  }
  // Use generalized map functions to convert from an arbitrary map.
  struct MapType type = GetMapType(ctx, field);
  if (type.key == NULL) return NULL;
  upb_Map* map = NewMap(ctx, arena, type);
  if (map == NULL) return NULL;
  struct PutMapElementContext child_ctx = {ctx.globals, arena, type, map};
  emacs_value fun = MakeFunction(ctx, 2, 2, PutMapElement, NULL, &child_ctx);
  FuncallSymbol2(ctx, kMapDo, fun, value);
  return Success(ctx) ? map : NULL;
}

// Updates the destination map with entries from the source map, overriding
// entries in the destination.
static bool UpdateMapEntries(struct Context ctx, upb_Arena* arena,
                             upb_Map* dest, const upb_Map* src) {
  size_t iter = kUpb_Map_Begin;
  upb_MessageValue key;
  upb_MessageValue value;
  while (upb_Map_Next(src, &key, &value, &iter)) {
    if (SetMapEntry(ctx, arena, dest, key, value) ==
        kUpb_MapInsertStatus_OutOfMemory) {
      return false;
    }
  }
  return true;
}

/// Looking up definitions

// Returns a message definition given its full protocol buffer name including
// the package name.  The containing file must have been registered using
// RegisterFileDescriptor before.
static const upb_MessageDef* FindMessageByFullName(struct Context ctx,
                                                   upb_StringView full_name) {
  const upb_MessageDef* def = upb_DefPool_FindMessageByNameWithSize(
      DefPool(ctx), full_name.data, full_name.size);
  if (def == NULL) UnknownMessageType(ctx, full_name);
  return def;
}

// Returns a message definition given its Lisp structure name as a symbol.  The
// containing file must have been registered using RegisterFileDescriptor
// before, and the Lisp structure must have been defined in a generated file.
static const upb_MessageDef* FindMessageByStructName(struct Context ctx,
                                                     emacs_value struct_name) {
  struct Allocator alloc = HeapAllocator();
  struct MutableString full_name = SymbolName(ctx, alloc, struct_name);
  if (full_name.data == NULL) return NULL;
  // This transformation must match the code in generate.el.
  ReplaceChar(full_name, '/', '.');
  const upb_MessageDef* def = FindMessageByFullName(ctx, View(full_name));
  Free(alloc, full_name.data);
  return def;
}

// Returns the full name of a message type given its type URL.  This is useful
// for dealing with the Any message type; see
// https://developers.google.com/protocol-buffers/docs/proto3#any and the
// comments in any.proto.
static upb_StringView MessageNameFromTypeUrl(struct Context ctx,
                                             upb_StringView type_url) {
  upb_StringView null = UPB_STRINGVIEW_INIT(NULL, 0);
  // The shortest syntactically valid type URL has at least three characters
  // (one-character domain, slash, one-character type name).
  if (type_url.size < 3) {
    WrongTypeArgument(ctx, kTypeUrlP, MakeString(ctx, type_url));
    return null;
  }
  // See the comment about unpack methods in any.proto.  We look for the last
  // slash character.  We could use memrchr here, but that isn’t portable.
  for (size_t i = type_url.size - 1; i > 1; --i) {
    if (type_url.data[i - 1] == '/') {
      return upb_StringView_FromDataAndSize(type_url.data + i,
                                            type_url.size - i);
    }
  }
  WrongTypeArgument(ctx, kTypeUrlP, MakeString(ctx, type_url));
  return null;
}

// Returns a message definition given its type URL.  The containing file must
// have been registered using RegisterFileDescriptor before.  This is useful for
// dealing with the Any message type; see
// https://developers.google.com/protocol-buffers/docs/proto3#any and the
// comments in any.proto.
static const upb_MessageDef* FindMessageByTypeUrl(struct Context ctx,
                                                  upb_StringView type_url) {
  upb_StringView full_name = MessageNameFromTypeUrl(ctx, type_url);
  if (full_name.data == NULL) return NULL;
  return FindMessageByFullName(ctx, full_name);
}

// Returns the type URL for a message type.  This is useful for dealing with the
// Any message type; see
// https://developers.google.com/protocol-buffers/docs/proto3#any and the
// comments in any.proto.
static struct MutableString TypeUrl(struct Context ctx, struct Allocator alloc,
                                    const upb_MessageDef* def) {
  upb_StringView prefix = upb_StringView_FromString("type.googleapis.com/");
  upb_StringView full_name =
      upb_StringView_FromString(upb_MessageDef_FullName(def));
  return Concat2(ctx, alloc, prefix, full_name);
}

// Returns the definition of the unqualified field name in the given message
// type.  Signals an error if there’s no such field.
static const upb_FieldDef* FindFieldDef(struct Context ctx,
                                        const upb_MessageDef* def,
                                        upb_StringView name) {
  const upb_FieldDef* field =
      upb_MessageDef_FindFieldByNameWithSize(def, name.data, name.size);
  if (field == NULL) UnknownField(ctx, def, name);
  return field;
}

// Returns the definition of the unqualified field name in the given message
// type.  The field name is given as a Lisp symbol.  Signals an error if there’s
// no such field.
static const upb_FieldDef* FindFieldBySymbol(struct Context ctx,
                                             const upb_MessageDef* def,
                                             emacs_value symbol) {
  struct Allocator alloc = HeapAllocator();
  struct MutableString name = SymbolName(ctx, alloc, symbol);
  if (name.data == NULL || name.size < 1) {
    Free(alloc, name.data);
    WrongTypeArgument(ctx, kFieldNameP, symbol);
    return NULL;
  }
  const upb_FieldDef* field = FindFieldDef(ctx, def, View(name));
  Free(alloc, name.data);
  return field;
}

// Returns the definition of the unqualified field name in the given message
// type.  The field name is given as a Lisp keyword; the leading colon is
// stripped.  Signals an error if there’s no such field.
static const upb_FieldDef* FindFieldByKeyword(struct Context ctx,
                                              const upb_MessageDef* def,
                                              emacs_value keyword) {
  struct Allocator alloc = HeapAllocator();
  struct MutableString name = SymbolName(ctx, alloc, keyword);
  if (name.data == NULL || name.size < 2 || name.data[0] != ':') {
    Free(alloc, name.data);
    WrongTypeArgument(ctx, kFieldKeywordP, keyword);
    return NULL;
  }
  const upb_FieldDef* field = FindFieldDef(
      ctx, def, upb_StringView_FromDataAndSize(name.data + 1, name.size - 1));
  Free(alloc, name.data);
  return field;
}

/// Type-erased field values

// Attempts to convert a field value from a compatible Lisp object to the given
// type.
static upb_MessageValue AdoptValue(struct Context ctx, upb_Arena* arena,
                                   const upb_FieldDef* type,
                                   emacs_value value) {
  if (upb_FieldDef_IsMap(type)) {
    upb_MessageValue ret;
    ret.map_val = AdoptMap(ctx, arena, type, value);
    return ret;
  }
  if (upb_FieldDef_IsRepeated(type)) {
    upb_MessageValue ret;
    ret.array_val = AdoptSequence(ctx, arena, type, value);
    return ret;
  }
  return AdoptSingular(ctx, arena, type, value);
}

// Sets the given message field to a new value converted from a Lisp object.
// Returns whether this was successful.
static bool SetFieldValue(struct Context ctx, upb_Arena* arena,
                          upb_Message* msg, const upb_FieldDef* field,
                          emacs_value value) {
  upb_MessageValue val = AdoptValue(ctx, arena, field, value);
  if (!Success(ctx)) return false;
  bool ok = upb_Message_SetFieldByDef(msg, field, val, arena);
  if (!ok) MemoryFull(ctx);
  return ok;
}

/// Time support

// Sets the fields in a Timestamp protocol buffer message from the given Emacs
// time value.  See Info node ‘(elisp) Time of Day’ for the specification of a
// time value.
static bool SetTimestampProto(struct Context ctx,
                              google_protobuf_Timestamp* msg,
                              emacs_value value) {
  struct timespec time = ExtractTime(ctx, value);
  if (!Success(ctx)) return false;
  assert(time.tv_nsec >= 0 && time.tv_nsec < 1000000000L);
  // Valid timestamp values are restricted in their range; see the comments in
  // timestamp.proto.  You can generate the magic numbers below using e.g. the
  // following Lisp form:
  //
  // (dolist (time '("0001-01-01T00:00:00Z" "9999-12-31T23:59:59Z"))
  //   (print (time-convert (parse-iso8601-time-string time) 'integer)))
  if (time.tv_sec < -62135596800 || time.tv_sec > 253402300799) {
    Signal1(ctx, kTimestampOverflow, value);
    return false;
  }
  // The canonical form of a timespec is compatible with the restrictions on a
  // Timestamp message.
  google_protobuf_Timestamp_set_seconds(msg, time.tv_sec);
  google_protobuf_Timestamp_set_nanos(msg, (int32_t)time.tv_nsec);
  return true;
}

// Sets the fields in a Duration protocol buffer message from the given Emacs
// time value.  See Info node ‘(elisp) Time of Day’ for the specification of a
// time value.
static bool SetDurationProto(struct Context ctx, google_protobuf_Duration* msg,
                             emacs_value value) {
  struct timespec time = ExtractTime(ctx, value);
  if (!Success(ctx)) return false;
  // Valid duration values are restricted in their range; see the comments in
  // duration.proto.
  intmax_t seconds = time.tv_sec;
  long nanos = time.tv_nsec;
  assert(nanos >= 0 && nanos < 1000000000L);
  // The seconds and nanos fields may not be of opposite signs; see the comments
  // in duration.proto.
  if (seconds < 0 && nanos > 0) {
    ++seconds;
    nanos -= 1000000000L;
  }
  // Ranges are taken straight from duration.proto.  We check the seconds range
  // only now because the normalization above might have changed it.
  if (seconds < -315576000000 || seconds > 315576000000) {
    Signal1(ctx, kDurationOverflow, value);
    return false;
  }
  assert(seconds == 0 || (seconds < 0 && nanos <= 0) ||
         (seconds > 0 && nanos >= 0));
  assert(nanos >= -999999999 && nanos <= 999999999);
  google_protobuf_Duration_set_seconds(msg, seconds);
  google_protobuf_Duration_set_nanos(msg, (int32_t)nanos);
  return true;
}

/// Descriptor support

// These are a few helper functions to deal with file descriptor sets.  We can’t
// have generate.el operate on generated Lisp reflections of those because that
// would create a dependency cycle.

// Parses a file descriptor from its serialized representation.
static const google_protobuf_FileDescriptorProto* ReadFileDescriptorProto(
    struct Context ctx, upb_Arena* arena, emacs_value serialized) {
  struct MutableString string =
      ExtractUnibyteString(ctx, ArenaAllocator(arena), serialized);
  if (string.data == NULL) return NULL;
  const google_protobuf_FileDescriptorProto* file =
      google_protobuf_FileDescriptorProto_parse_ex(
          string.data, string.size, NULL,
          kUpb_DecodeOption_AliasString | kUpb_DecodeOption_CheckRequired,
          arena);
  if (file == NULL) {
    WrongTypeArgument(ctx, kSerializedFileDescriptorProtoP, serialized);
  }
  return file;
}

// Helper function for ParseCodeGeneratorRequest.
static emacs_value SerializeFileDescriptor(
    struct Context ctx, upb_Arena* arena,
    const google_protobuf_FileDescriptorProto* file) {
  size_t size;
  const char* data = google_protobuf_FileDescriptorProto_serialize_ex(
      file, kUpb_EncodeOption_Deterministic | kUpb_EncodeOption_CheckRequired,
      arena, &size);
  if (data == NULL) {
    Signal0(ctx, kSerializeError);
    return NULL;
  }
  return MakeUnibyteString(ctx, upb_StringView_FromDataAndSize(data, size));
}

// Helper function for ParseCodeGeneratorRequest.
static emacs_value ConvertDependencies(struct Context ctx, upb_Arena* arena,
                                       const upb_FileDef* file) {
  struct Allocator alloc = ArenaAllocator(arena);
  int size = upb_FileDef_DependencyCount(file);
  assert(size >= 0);
  if (size == 0) return Nil(ctx);
  emacs_value* array = AllocateLispArray(ctx, alloc, (size_t)size);
  if (array == NULL) return NULL;
  for (int i = 0; i < size; ++i) {
    const upb_FileDef* dep = upb_FileDef_Dependency(file, i);
    upb_StringView name = upb_StringView_FromString(upb_FileDef_Name(dep));
    array[i] = MakeString(ctx, name);
  }
  return List(ctx, size, array);
}

// Helper function for ParseCodeGeneratorRequest.  Returns a list of field names
// as symbols.
static emacs_value ConvertFieldDescriptors(struct Context ctx, upb_Arena* arena,
                                           const upb_MessageDef* message) {
  struct Allocator alloc = ArenaAllocator(arena);
  int count = upb_MessageDef_FieldCount(message);
  assert(count >= 0);
  if (count == 0) return Nil(ctx);
  emacs_value* args = AllocateLispArray(ctx, alloc, (size_t)count);
  if (args == NULL) return NULL;
  for (int i = 0; i < count; ++i) {
    const upb_FieldDef* field = upb_MessageDef_Field(message, i);
    upb_StringView name = upb_StringView_FromString(upb_FieldDef_Name(field));
    args[i] = Intern(ctx, name);
  }
  return List(ctx, count, args);
}

// Helper function for ParseCodeGeneratorRequest.  Returns a list of
// (name value) pairs.
static emacs_value ConvertEnumValueDescriptors(struct Context ctx,
                                               upb_Arena* arena,
                                               const upb_EnumDef* enumeration) {
  struct Allocator alloc = ArenaAllocator(arena);
  int count = upb_EnumDef_ValueCount(enumeration);
  assert(count >= 0);
  if (count == 0) return Nil(ctx);
  emacs_value* args = AllocateLispArray(ctx, alloc, (size_t)count);
  if (args == NULL) return NULL;
  for (int i = 0; i < count; ++i) {
    const upb_EnumValueDef* value = upb_EnumDef_Value(enumeration, i);
    upb_StringView name =
        upb_StringView_FromString(upb_EnumValueDef_Name(value));
    int32_t number = upb_EnumValueDef_Number(value);
    args[i] = List2(ctx, Intern(ctx, name), MakeInteger(ctx, number));
  }
  return List(ctx, count, args);
}

// Helper function for ParseCodeGeneratorRequest.  Adds new enumeration
// descriptors to the beginning of the given list.  The list elements are of the
// form (full-name (enumerator-name value)…), where ‘full-name’ is the full name
// of the enumeration type as a string, including the package name.
static void ConvertEnumDescriptors(struct Context ctx, upb_Arena* arena,
                                   int count,
                                   const upb_EnumDef* (*get)(const void*, int),
                                   const void* parent, emacs_value* list) {
  assert(count >= 0);
  for (int i = 0; i < count; ++i) {
    const upb_EnumDef* enumeration = get(parent, i);
    upb_StringView full_name =
        upb_StringView_FromString(upb_EnumDef_FullName(enumeration));
    emacs_value values = ConvertEnumValueDescriptors(ctx, arena, enumeration);
    emacs_value elt = Cons(ctx, MakeString(ctx, full_name), values);
    Push(ctx, elt, list);
  }
}

static const upb_MessageDef* NestedMessageDef(const void* parent, int i) {
  return upb_MessageDef_NestedMessage(parent, i);
}

static const upb_EnumDef* NestedEnumDef(const void* parent, int i) {
  return upb_MessageDef_NestedEnum(parent, i);
}

// Helper function for ParseCodeGeneratorRequest.  Adds new message and
// enumeration type descriptors to the beginnings of the given lists.  The
// message type list elements are of the form (full-name field-name…), and the
// enumeration list elements are of the form
// (full-name (enumerator-name value)…).  In both cases, ‘full-name’ is the full
// name of the type as a string, including the package name.
static void ConvertMessageDescriptors(
    struct Context ctx, upb_Arena* arena, int count,
    const upb_MessageDef* (*get)(const void*, int), const void* parent,
    emacs_value* messages_list, emacs_value* enums_list) {
  assert(count >= 0);
  for (int i = 0; i < count; ++i) {
    const upb_MessageDef* message = get(parent, i);
    upb_StringView full_name =
        upb_StringView_FromString(upb_MessageDef_FullName(message));
    emacs_value fields = ConvertFieldDescriptors(ctx, arena, message);
    emacs_value elt = Cons(ctx, MakeString(ctx, full_name), fields);
    Push(ctx, elt, messages_list);
    int messages_count = upb_MessageDef_NestedMessageCount(message);
    ConvertMessageDescriptors(ctx, arena, messages_count, NestedMessageDef,
                              message, messages_list, enums_list);
    int enums_count = upb_MessageDef_NestedEnumCount(message);
    ConvertEnumDescriptors(ctx, arena, enums_count, NestedEnumDef, message,
                           enums_list);
  }
}

static const upb_FileDef* AddFileToPool(
    struct Context ctx, const google_protobuf_FileDescriptorProto* file,
    upb_DefPool* pool) {
  upb_Status status;
  upb_Status_Clear(&status);
  const upb_FileDef* ret = upb_DefPool_AddFile(pool, file, &status);
  if (ret == NULL || !upb_Status_IsOk(&status)) {
    ProtoError(ctx, kRegistrationFailed, &status);
    return NULL;
  }
  return ret;
}

static const upb_MessageDef* TopLevelMessageDef(const void* file, int i) {
  return upb_FileDef_TopLevelMessage(file, i);
}

static const upb_EnumDef* TopLevelEnumDef(const void* file, int i) {
  return upb_FileDef_TopLevelEnum(file, i);
}

// Helper function for ParseCodeGeneratorRequest.  Returns a list of the form
// (proto-file-name serialized-file-descriptor-proto
//  (dependency-file-name…)
//  ((message-name field-name…)…)
//  ((enumeration-name (enumerator-name value)…)…)).
static emacs_value ConvertFileDescriptor(
    struct Context ctx, upb_Arena* arena, const upb_FileDef* file,
    const google_protobuf_FileDescriptorProto* proto) {
  emacs_value name =
      MakeString(ctx, upb_StringView_FromString(upb_FileDef_Name(file)));
  emacs_value serialized = SerializeFileDescriptor(ctx, arena, proto);
  emacs_value dependencies = ConvertDependencies(ctx, arena, file);
  emacs_value messages_list = Nil(ctx);
  emacs_value enums_list = Nil(ctx);
  int messages_count = upb_FileDef_TopLevelMessageCount(file);
  ConvertMessageDescriptors(ctx, arena, messages_count, TopLevelMessageDef,
                            file, &messages_list, &enums_list);
  int enums_count = upb_FileDef_TopLevelEnumCount(file);
  ConvertEnumDescriptors(ctx, arena, enums_count, TopLevelEnumDef, file,
                         &enums_list);
  // The helper functions have constructed the lists in reversed order.
  messages_list = Nreverse(ctx, messages_list);
  enums_list = Nreverse(ctx, enums_list);
  return List5(ctx, name, serialized, dependencies, messages_list, enums_list);
}

struct FileDefProtoPair {
  const upb_FileDef* def;
  const google_protobuf_FileDescriptorProto* proto;
};

// Helper function for ConvertGeneratorRequest.  The arguments must point to
// FileDefProtoPair structures.
int CompareFileDefProtoPairs(const void* a, const void* b) {
  const struct FileDefProtoPair* p = a;
  const struct FileDefProtoPair* q = b;
  uintptr_t u = (uintptr_t)p->def;
  uintptr_t v = (uintptr_t)q->def;
  if (u < v) return -1;
  if (u > v) return +1;
  return 0;
}

// Returns a list of the form
// ((proto-file-name serialized-file-descriptor-proto
//   (dependency-file-name…)
//   ((message-name field-name…)…)
//   ((enumeration-name (enumerator-name value)…)…))…).
static emacs_value ConvertGeneratorRequest(
    struct Context ctx, upb_Arena* arena, upb_DefPool* pool,
    const google_protobuf_compiler_CodeGeneratorRequest* request) {
  struct Allocator alloc = ArenaAllocator(arena);
  size_t protos_count;
  const google_protobuf_FileDescriptorProto* const* protos =
      google_protobuf_compiler_CodeGeneratorRequest_proto_file(request,
                                                               &protos_count);
  struct FileDefProtoPair* pairs =
      AllocateArray(ctx, alloc, protos_count, sizeof *pairs);
  if (pairs == NULL && protos_count > 0) return NULL;
  for (size_t i = 0; i < protos_count; ++i) {
    const google_protobuf_FileDescriptorProto* proto = protos[i];
    const upb_FileDef* def = AddFileToPool(ctx, proto, pool);
    if (def == NULL) return false;
    struct FileDefProtoPair* pair = &pairs[i];
    pair->def = def;
    pair->proto = proto;
  }
  qsort(pairs, protos_count, sizeof *pairs, CompareFileDefProtoPairs);
  size_t names_count;
  const upb_StringView* names =
      google_protobuf_compiler_CodeGeneratorRequest_file_to_generate(
          request, &names_count);
  if (names_count > PTRDIFF_MAX) {
    OverflowError1(ctx, MakeUInteger(ctx, names_count));
    return NULL;
  }
  emacs_value* array = AllocateLispArray(ctx, alloc, names_count);
  if (array == NULL && names_count > 0) return NULL;
  for (size_t i = 0; i < names_count; ++i) {
    upb_StringView name = names[i];
    const upb_FileDef* def =
        upb_DefPool_FindFileByNameWithSize(pool, name.data, name.size);
    if (def == NULL) {
      Signal1(ctx, kUnknownProtoFile, MakeString(ctx, name));
      return NULL;
    }
    struct FileDefProtoPair key = {def, NULL};
    const struct FileDefProtoPair* pair = bsearch(
        &key, pairs, protos_count, sizeof *pairs, CompareFileDefProtoPairs);
    if (pair == NULL) {
      Signal1(ctx, kUnknownProtoFile, MakeString(ctx, name));
      return NULL;
    };
    array[i] = ConvertFileDescriptor(ctx, arena, def, pair->proto);
  }
  return List(ctx, (ptrdiff_t)names_count, array);
}

static emacs_value ParseGeneratorRequest(struct Context ctx, upb_Arena* arena,
                                         upb_StringView serialized) {
  google_protobuf_compiler_CodeGeneratorRequest* request =
      google_protobuf_compiler_CodeGeneratorRequest_parse_ex(
          serialized.data, serialized.size, NULL,
          kUpb_DecodeOption_AliasString | kUpb_DecodeOption_CheckRequired,
          arena);
  if (request == NULL) {
    WrongTypeArgument(ctx, kSerializedCodeGeneratorRequestP,
                      MakeString(ctx, serialized));
    return NULL;
  }
  size_t count;
  google_protobuf_FileDescriptorProto* const* files =
      google_protobuf_compiler_CodeGeneratorRequest_mutable_proto_file(request,
                                                                       &count);
  for (size_t i = 0; i < count; ++i) {
    // Strip unnecessary documentation.
    google_protobuf_FileDescriptorProto_clear_source_code_info(files[i]);
  }
  upb_DefPool* pool = upb_DefPool_New();
  if (pool == NULL) {
    MemoryFull(ctx);
    return NULL;
  }
  emacs_value ret = ConvertGeneratorRequest(ctx, arena, pool, request);
  upb_DefPool_Free(pool);
  return ret;
}

static upb_StringView SerializeGeneratorResponse(struct Context ctx,
                                                 upb_Arena* arena,
                                                 emacs_value list) {
  upb_StringView null = UPB_STRINGVIEW_INIT(NULL, 0);
  struct Allocator alloc = ArenaAllocator(arena);
  google_protobuf_compiler_CodeGeneratorResponse* resp =
      google_protobuf_compiler_CodeGeneratorResponse_new(arena);
  if (resp == NULL || !Success(ctx)) {
    MemoryFull(ctx);
    return null;
  }
  while (IsNotNil(ctx, list)) {
    emacs_value elt = Pop(ctx, &list);
    upb_StringView name = View(ExtractString(ctx, alloc, Car(ctx, elt)));
    upb_StringView content = View(ExtractString(ctx, alloc, Cdr(ctx, elt)));
    google_protobuf_compiler_CodeGeneratorResponse_File* file =
        google_protobuf_compiler_CodeGeneratorResponse_add_file(resp, arena);
    if (file == NULL) {
      MemoryFull(ctx);
      return null;
    }
    google_protobuf_compiler_CodeGeneratorResponse_File_set_name(file, name);
    google_protobuf_compiler_CodeGeneratorResponse_File_set_content(file,
                                                                    content);
  }
  size_t size;
  const char* data =
      google_protobuf_compiler_CodeGeneratorResponse_serialize_ex(
          resp,
          kUpb_EncodeOption_Deterministic | kUpb_EncodeOption_CheckRequired,
          arena, &size);
  if (data == NULL) {
    Signal0(ctx, kSerializeError);
    return null;
  }
  return upb_StringView_FromDataAndSize(data, size);
}

static bool RegisterFileDescriptorProto(
    struct Context ctx, const google_protobuf_FileDescriptorProto* file) {
  upb_DefPool* pool = MutableDefPool(ctx);
  upb_StringView name = google_protobuf_FileDescriptorProto_name(file);
  if (upb_DefPool_FindFileByNameWithSize(pool, name.data, name.size)) {
    // Duplicate registrations result in an error.  We allow them because a
    // generated file might be legitimately reloaded, or we might have
    // preregistered the file descriptor.
    return true;
  }
  return AddFileToPool(ctx, file, pool) != NULL;
}

/// Operating system interfaces

#ifdef _WIN32
static void FileError(struct Context ctx, DWORD code) {
  emacs_value message = Nil(ctx);
  enum { kWideSize = 0x4000 };
  wchar_t wide_buf[kWideSize];
  DWORD wide_len =
      FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                     NULL, code, 0, wide_buf, kWideSize, NULL);
  if (wide_len > 0) {
    enum { kUtf8Size = kWideSize * 3 };
    char utf8_buf[kUtf8Size];
    assert(wide_len <= INT_MAX);
    int utf8_len = WideCharToMultiByte(CP_UTF8, 0, wide_buf, (int)wide_len,
                                       utf8_buf, kUtf8Size, NULL, NULL);
    if (utf8_len > 0) {
      message = MakeString(
          ctx, upb_StringView_FromDataAndSize(utf8_buf, (size_t)utf8_len));
    }
  }
  Signal2(ctx, kFileError, MakeUInteger(ctx, code), message);
}

typedef HANDLE FileHandle;

static bool ValidHandle(HANDLE handle) {
  return handle != INVALID_HANDLE_VALUE && handle != NULL;
}

static HANDLE StandardHandle(struct Context ctx, DWORD id) {
  HANDLE handle = GetStdHandle(id);
  if (handle == INVALID_HANDLE_VALUE) FileError(ctx, GetLastError());
  if (handle == NULL) FileError(ctx, ERROR_INVALID_HANDLE);
  return handle;
}

static HANDLE StdinHandle(struct Context ctx) {
  return StandardHandle(ctx, STD_INPUT_HANDLE);
}

static HANDLE StdoutHandle(struct Context ctx) {
  return StandardHandle(ctx, STD_OUTPUT_HANDLE);
}

static upb_StringView ReadHandle(struct Context ctx, HANDLE handle,
                                 char* buffer, size_t size) {
  upb_StringView null = UPB_STRINGVIEW_INIT(NULL, 0);
  if (!Success(ctx)) return null;
  DWORD n;
  if (!ReadFile(handle, buffer, size <= MAXDWORD ? (DWORD)size : MAXDWORD, &n,
                NULL)) {
    assert(n == 0);
    DWORD code = GetLastError();
    if (code != ERROR_BROKEN_PIPE) FileError(ctx, code);
    return null;
  }
  assert(n <= size);
  if (n == 0) return null;
  return upb_StringView_FromDataAndSize(buffer, n);
}

static bool WriteHandle(struct Context ctx, HANDLE handle,
                        upb_StringView* data) {
  if (!Success(ctx)) return false;
  DWORD n;
  if (!WriteFile(handle, data->data,
                 data->size <= MAXDWORD ? (DWORD)data->size : MAXDWORD, &n,
                 NULL)) {
    assert(n == 0);
    FileError(ctx, GetLastError());
    return false;
  }
  data->data += n;
  data->size -= n;
  return true;
}
#else
static void FileError(struct Context ctx, int code) {
  emacs_value message = Nil(ctx);
  char buffer[0x4000];
  // Make sure we get the POSIX version of strerror_r by explicitly assigning
  // its return value to an int variable.  See the strerror_r(3) man page.
  int result = strerror_r(code, buffer, sizeof buffer);
  if (result == 0) {
    message = MakeString(ctx, upb_StringView_FromString(buffer));
  }
  Signal2(ctx, kFileError, MakeInteger(ctx, code), message);
}

typedef int FileHandle;

static bool ValidHandle(int fd) {
  return fd >= 0;
}

static int StdinHandle(struct Context ctx ABSL_ATTRIBUTE_UNUSED) {
  return STDIN_FILENO;
}

static int StdoutHandle(struct Context ctx ABSL_ATTRIBUTE_UNUSED) {
  return STDOUT_FILENO;
}

static upb_StringView ReadHandle(struct Context ctx, int fd, char* buffer,
                                 size_t size) {
  upb_StringView null = UPB_STRINGVIEW_INIT(NULL, 0);
  if (!Success(ctx)) return null;
  ssize_t n = read(fd, buffer, size <= SSIZE_MAX ? size : SSIZE_MAX);
  if (n < 0) {
    FileError(ctx, errno);
    return null;
  }
  if (n == 0) return null;
  assert((size_t)n <= size);
  return upb_StringView_FromDataAndSize(buffer, (size_t)n);
}

static bool WriteHandle(struct Context ctx, int fd, upb_StringView* data) {
  if (!Success(ctx)) return false;
  ssize_t n = write(fd, data->data, data->size <= kMaxIO ? data->size : kMaxIO);
  if (n < 0) {
    FileError(ctx, errno);
    return false;
  }
  data->data += n;
  data->size -= (size_t)n;
  return true;
}
#endif

/// Function definitions

enum { kMaxParams = 4 };

struct Parameters {
  enum GlobalSymbol types[kMaxParams];
};

static struct Parameters Params0(void) {
  struct Parameters ret = {{kNil}};
  return ret;
}

static struct Parameters Params1(enum GlobalSymbol a) {
  struct Parameters ret = {{a, kNil}};
  return ret;
}

static struct Parameters Params2(enum GlobalSymbol a, enum GlobalSymbol b) {
  struct Parameters ret = {{a, b, kNil}};
  return ret;
}

static struct Parameters Params3(enum GlobalSymbol a, enum GlobalSymbol b,
                                 enum GlobalSymbol c) {
  struct Parameters ret = {{a, b, c, kNil}};
  return ret;
}

static struct Parameters Params4(enum GlobalSymbol a, enum GlobalSymbol b,
                                 enum GlobalSymbol c, enum GlobalSymbol d) {
  struct Parameters ret = {{a, b, c, d}};
  return ret;
}

static emacs_value FunctionType(struct Context ctx, struct Parameters params,
                                enum GlobalSymbol return_type) {
  ptrdiff_t num_params;
  emacs_value param_types[kMaxParams];
  for (num_params = 0; num_params < kMaxParams; ++num_params) {
    enum GlobalSymbol type = params.types[num_params];
    if (type == kNil) break;
    param_types[num_params] = GlobalSymbol(ctx, type);
  }
  assert(return_type != kNil);
  return List3(ctx, GlobalSymbol(ctx, kFunction),
               List(ctx, num_params, param_types),
               GlobalSymbol(ctx, return_type));
}

enum Declarations { kDefault = 0, kNoSideEffects = 0x01 };

static void FunctionPut(struct Context ctx, emacs_value function,
                        enum GlobalSymbol property, emacs_value value) {
  FuncallSymbol3(ctx, kFunctionPut, function, GlobalSymbol(ctx, property),
                 value);
}

// Defines an exported function, like ‘defun’.  The name may only contain ASCII
// characters.  The documentation string is required.  If the function accepts
// arguments, the documentation string must also contain a calling convention;
// see Info node ‘(elisp) Function Documentation’.  The exported functions will
// receive a valid pointer to a Globals structure as data pointer.
static void Defun(struct Context ctx, const char* name, ptrdiff_t min_arity,
                  ptrdiff_t max_arity, const char* doc,
                  struct Parameters params, enum GlobalSymbol return_type,
                  enum Declarations decls, Function fun) {
  assert(doc != NULL && strlen(doc) > 0);
  assert((max_arity == 0) !=
         (strstr(doc, "\n\n(") != NULL && doc[strlen(doc) - 1] == ')'));
  emacs_value symbol = InternAscii(ctx, name);
  emacs_value function = MakeFunction(ctx, min_arity, max_arity, fun, doc,
                                      (struct Globals*)ctx.globals);
  FuncallSymbol2(ctx, kDefalias, symbol, function);
  FuncallSymbol3(ctx, kFunctionPut, symbol, GlobalSymbol(ctx, kFunctionType),
                 FunctionType(ctx, params, return_type));
  if (decls & kNoSideEffects) {
    FunctionPut(ctx, symbol, kSideEffectFree, GlobalSymbol(ctx, kT));
  }
}

// Returns args[index] if that is valid, otherwise return nil.
static emacs_value OptionalArg(struct Context ctx, ptrdiff_t nargs,
                               emacs_value* args, ptrdiff_t index) {
  return index < nargs ? args[index] : Nil(ctx);
}

// Specifies a keyword argument.
struct KeySpec {
  // Keyword to check.
  enum GlobalSymbol key;
  // The function to be called upon encountering the keyword argument.  The
  // function is called with four arguments: a live context, the argument
  // value, and the ‘iarg’ and ‘parg’ fields.
  void (*func)(struct Context, emacs_value, int, void*);
  // Additional arguments to ‘func’.
  int iarg;
  void* parg;
};

// Signals an error that a keyword argument is unknown.
static void UnknownKey(struct Context ctx, ptrdiff_t nspecs,
                       const struct KeySpec* specs, emacs_value arg) {
  struct Allocator alloc = HeapAllocator();
  assert(nspecs > 0);
  emacs_value* choices = AllocateLispArray(ctx, alloc, (size_t)nspecs);
  if (choices == NULL) return;
  for (ptrdiff_t i = 0; i < nspecs; ++i) {
    choices[i] = GlobalSymbol(ctx, specs[i].key);
  }
  WrongChoice(ctx, arg, nspecs, choices);
  Free(alloc, choices);
}

// Matches the given keyword against an array of key specifications.  Returns
// the index into the array, or a negative number if the keyword is unknown.
static ptrdiff_t FindKey(struct Context ctx, ptrdiff_t nspecs,
                         const struct KeySpec* specs, emacs_value arg) {
  for (ptrdiff_t i = 0; i < nspecs; ++i) {
    if (EqGlobal(ctx, arg, specs[i].key)) return i;
  }
  UnknownKey(ctx, nspecs, specs, arg);
  return -1;
}

// Parses keyword arguments.  Calls the functions specified in the ‘specs’ array
// for each actual keyword argument.  Returns ‘false’ if something went wrong.
ABSL_MUST_USE_RESULT static bool ParseKeys(struct Context ctx, ptrdiff_t nspecs,
                                           const struct KeySpec* specs,
                                           ptrdiff_t nargs, emacs_value* args) {
  assert(nargs >= 0);
  if (nargs % 2 != 0) {
    WrongTypeArgument(ctx, kPlistp, List(ctx, nargs, args));
    return false;
  }
  assert(nspecs <= 32);
  uint32_t seen = 0;
  for (ptrdiff_t i = 0; i < nargs; i += 2) {
    ptrdiff_t j = FindKey(ctx, nspecs, specs, args[i]);
    if (j < 0) return false;
    assert(j < nspecs);
    struct KeySpec spec = specs[j];
    uint32_t bit = UINT32_C(1) << j;
    if (seen & bit) {
      DuplicateKey(ctx, GlobalSymbol(ctx, spec.key));
      return false;
    }
    seen |= bit;
    spec.func(ctx, args[i + 1], spec.iarg, spec.parg);
  }
  return true;
}

static void ToggleBit(void* ptr, int bit, bool set) {
  int* val = ptr;
  if (set) {
    *val |= bit;
  } else {
    *val &= ~bit;
  }
}

static void SetBit(struct Context ctx, emacs_value value, int bit, void* ptr) {
  ToggleBit(ptr, bit, IsNotNil(ctx, value));
}

static void ClearBit(struct Context ctx, emacs_value value, int bit,
                     void* ptr) {
  ToggleBit(ptr, bit, IsNil(ctx, value));
}

static void DefineError(struct Context ctx, enum GlobalSymbol symbol,
                        const char* message, enum GlobalSymbol parent) {
  FuncallSymbol3(ctx, kDefineError, GlobalSymbol(ctx, symbol),
                 MakeString(ctx, upb_StringView_FromString(message)),
                 GlobalSymbol(ctx, parent));
}

static void Provide(struct Context ctx, const char* feature) {
  Funcall1(ctx, InternAscii(ctx, "provide"), InternAscii(ctx, feature));
}

/// Exported functions

// All of these functions should be defined using Defun.  The names of these
// functions are the Lisp names with the prefix ‘elisp/proto/’ stripped.  The
// first thing they do is initialize a Context structure.  Then, they extract
// arguments using the helper functions defined above.  See the top-level
// comments for some discussion around the expected implementation behavior of
// these functions.
//
// Mutating functions such as ‘elisp/proto/sort-array’ and
// ‘elisp/proto/nreverse-array’ typically return nil instead of their mutated
// argument.  This gives readers a hint that they mutate their argument in place
// instead of returning a copy.

static emacs_value Make(emacs_env* env, ptrdiff_t nargs, emacs_value* args,
                        void* data) {
  struct Context ctx = {env, data};
  assert(nargs >= 1);
  if (nargs % 2 == 0) {
    WrongTypeArgument(ctx, kPlistp, List(ctx, nargs - 1, args + 1));
    return NULL;
  }
  struct LispArena arena = MakeArena(ctx);
  const upb_MessageDef* def = FindMessageByStructName(ctx, args[0]);
  if (def == NULL || arena.ptr == NULL) return NULL;
  upb_Message* msg = NewMessage(ctx, arena.ptr, def);
  if (msg == NULL) return NULL;
  for (ptrdiff_t i = 1; i < nargs; i += 2) {
    const upb_FieldDef* field = FindFieldByKeyword(ctx, def, args[i]);
    if (field == NULL) return NULL;
    if (!SetFieldValue(ctx, arena.ptr, msg, field, args[i + 1])) return NULL;
  }
  return MakeMutableMessage(ctx, arena, def, msg);
}

static emacs_value Parse(emacs_env* env, ptrdiff_t nargs, emacs_value* args,
                         void* data) {
  struct Context ctx = {env, data};
  assert(nargs >= 2 && nargs <= 4);
  struct LispArena arena = MakeArena(ctx);
  if (arena.ptr == NULL) return NULL;
  emacs_value full_name = args[0];
  struct MutableString serialized =
      ExtractUnibyteString(ctx, ArenaAllocator(arena.ptr), args[1]);
  if (serialized.data == NULL) return NULL;
  int options = kUpb_DecodeOption_CheckRequired;
  const struct KeySpec spec = {kCAllowPartial, ClearBit,
                               kUpb_DecodeOption_CheckRequired, &options};
  if (!ParseKeys(ctx, 1, &spec, nargs - 2, args + 2)) return NULL;
  const upb_MessageDef* def = FindMessageByStructName(ctx, full_name);
  if (def == NULL) return NULL;
  upb_Message* msg =
      ParseMessage(ctx, arena.ptr, def, View(serialized), options);
  if (msg == NULL) return NULL;
  return MakeMutableMessage(ctx, arena, def, msg);
}

static emacs_value ParseJson(emacs_env* env, ptrdiff_t nargs, emacs_value* args,
                             void* data) {
  struct Context ctx = {env, data};
  assert(nargs >= 2 && nargs <= 4);
  int options = 0;
  const struct KeySpec spec = {kCDiscardUnknown, SetBit,
                               upb_JsonDecode_IgnoreUnknown, &options};
  if (!ParseKeys(ctx, 1, &spec, nargs - 2, args + 2)) return NULL;
  struct LispArena arena = MakeArena(ctx);
  if (arena.ptr == NULL) return NULL;
  const upb_MessageDef* def = FindMessageByStructName(ctx, args[0]);
  if (def == NULL) return NULL;
  struct Allocator alloc = HeapAllocator();
  struct MutableString json = ExtractString(ctx, alloc, args[1]);
  if (json.data == NULL) return NULL;
  upb_Message* msg = ParseMessageJson(ctx, arena.ptr, def, View(json), options);
  Free(alloc, json.data);
  if (msg == NULL) return NULL;
  return MakeMutableMessage(ctx, arena, def, msg);
}

static emacs_value Serialize(emacs_env* env, ptrdiff_t nargs, emacs_value* args,
                             void* data) {
  struct Context ctx = {env, data};
  assert(nargs >= 1 && nargs <= 7);
  struct MessageArg msg = ExtractMessage(ctx, args[0]);
  if (msg.type == NULL) return NULL;
  int options = kUpb_EncodeOption_CheckRequired;
  const struct KeySpec specs[] = {
      {kCAllowPartial, ClearBit, kUpb_EncodeOption_CheckRequired, &options},
      {kCDiscardUnknown, SetBit, kUpb_EncodeOption_SkipUnknown, &options},
      {kCDeterministic, SetBit, kUpb_EncodeOption_Deterministic, &options}};
  if (!ParseKeys(ctx, 3, specs, nargs - 1, args + 1)) return NULL;
  upb_Arena* arena = NewArena(ctx);
  if (arena == NULL) return NULL;
  struct MutableString serialized =
      SerializeMessage(ctx, arena, msg.type, msg.value, options);
  if (serialized.data == NULL) {
    upb_Arena_Free(arena);
    return NULL;
  }
  emacs_value ret = MakeUnibyteString(ctx, View(serialized));
  upb_Arena_Free(arena);
  return ret;
}

static emacs_value SerializeText(emacs_env* env, ptrdiff_t nargs,
                                 emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs >= 1 && nargs <= 7);
  struct MessageArg msg = ExtractMessage(ctx, args[0]);
  if (msg.type == NULL) return NULL;
  int options = UPB_TXTENC_NOSORT;
  const struct KeySpec specs[] = {
      {kCCompact, SetBit, UPB_TXTENC_SINGLELINE, &options},
      {kCDiscardUnknown, SetBit, UPB_TXTENC_SKIPUNKNOWN, &options},
      {kCDeterministic, ClearBit, UPB_TXTENC_NOSORT, &options}};
  if (!ParseKeys(ctx, 3, specs, nargs - 1, args + 1)) return NULL;
  struct Allocator alloc = HeapAllocator();
  struct MutableString serialized =
      SerializeMessageText(ctx, alloc, msg.type, msg.value, options);
  if (serialized.data == NULL) return NULL;
  emacs_value ret = MakeString(ctx, View(serialized));
  Free(alloc, serialized.data);
  return ret;
}

static emacs_value SerializeJson(emacs_env* env, ptrdiff_t nargs,
                                 emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs >= 1 && nargs <= 5);
  struct MessageArg msg = ExtractMessage(ctx, args[0]);
  if (msg.type == NULL) return NULL;
  int options = 0;
  const struct KeySpec specs[] = {
      {kCEmitDefaults, SetBit, upb_JsonEncode_EmitDefaults, &options},
      {kCProtoNames, SetBit, upb_JsonEncode_UseProtoNames, &options}};
  if (!ParseKeys(ctx, 2, specs, nargs - 1, args + 1)) return NULL;
  struct Allocator alloc = HeapAllocator();
  struct MutableString serialized =
      SerializeMessageJson(ctx, alloc, msg.type, msg.value, options);
  if (serialized.data == NULL) return NULL;
  emacs_value ret = MakeString(ctx, View(serialized));
  Free(alloc, serialized.data);
  return ret;
}

static emacs_value PrintMessage(emacs_env* env, ptrdiff_t nargs,
                                emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1 || nargs == 2);
  struct MessageArg msg = ExtractMessage(ctx, args[0]);
  if (msg.type == NULL) return NULL;
  emacs_value stream = OptionalArg(ctx, nargs, args, 1);
  PrincLiteral(ctx, "#<protocol buffer message ", stream);
  PrincString(ctx, upb_StringView_FromString(upb_MessageDef_FullName(msg.type)),
              stream);
  PrincLiteral(ctx, " { ", stream);
  PrincFields(ctx, msg.type, msg.value, stream);
  PrincLiteral(ctx, "}>", stream);
  return Nil(ctx);
}

static emacs_value ClearMessage(emacs_env* env,
                                ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                                emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  struct MutableMessageArg msg = ExtractMutableMessage(ctx, args[0]);
  if (msg.type == NULL) return NULL;
  upb_Message_ClearByDef(msg.value, msg.type);
  return Nil(ctx);
}

static emacs_value MessageMutableP(emacs_env* env,
                                   ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                                   emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  struct LispMessage msg = ExtractMessageStruct(ctx, args[0]);
  if (msg.ptr == NULL) return NULL;
  return MakeBoolean(ctx, msg.ptr->mutable);
}

static emacs_value HasField(emacs_env* env,
                            ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                            emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2);
  struct MessageArg msg = ExtractMessage(ctx, args[0]);
  if (msg.type == NULL) return NULL;
  const upb_FieldDef* def = FindFieldBySymbol(ctx, msg.type, args[1]);
  if (def == NULL) return NULL;
  if (!upb_FieldDef_HasPresence(def)) {
    NoPresence(ctx, def);
    return NULL;
  }
  return MakeBoolean(ctx, upb_Message_HasFieldByDef(msg.value, def));
}

static emacs_value Field(emacs_env* env, ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                         emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2);
  struct MessageArg msg = ExtractMessage(ctx, args[0]);
  if (msg.type == NULL) return NULL;
  const upb_FieldDef* def = FindFieldBySymbol(ctx, msg.type, args[1]);
  if (def == NULL) return NULL;
  if (upb_FieldDef_IsMap(def)) {
    const upb_Map* value = upb_Message_GetFieldByDef(msg.value, def).map_val;
    return value == NULL ? Nil(ctx) : MakeMap(ctx, msg.arena, def, value);
  }
  if (upb_FieldDef_IsRepeated(def)) {
    const upb_Array* value =
        upb_Message_GetFieldByDef(msg.value, def).array_val;
    return value == NULL ? Nil(ctx) : MakeArray(ctx, msg.arena, def, value);
  }
  if (upb_FieldDef_IsSubMessage(def)) {
    if (!upb_Message_HasFieldByDef(msg.value, def)) return Nil(ctx);
    return MakeMessage(ctx, msg.arena, upb_FieldDef_MessageSubDef(def),
                       upb_Message_GetFieldByDef(msg.value, def).msg_val);
  }
  return MakeScalar(ctx, def, upb_Message_GetFieldByDef(msg.value, def));
}

static emacs_value MutableField(emacs_env* env,
                                ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                                emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2);
  struct MutableMessageArg msg = ExtractMutableMessage(ctx, args[0]);
  if (msg.type == NULL) return NULL;
  const upb_FieldDef* def = FindFieldBySymbol(ctx, msg.type, args[1]);
  if (def == NULL) return NULL;
  if (upb_FieldDef_IsMap(def)) {
    return MakeMutableMap(
        ctx, msg.arena, def,
        upb_Message_Mutable(msg.value, def, msg.arena.ptr).map);
  }
  if (upb_FieldDef_IsRepeated(def)) {
    return MakeMutableArray(
        ctx, msg.arena, def,
        upb_Message_Mutable(msg.value, def, msg.arena.ptr).array);
  }
  if (upb_FieldDef_IsSubMessage(def)) {
    return MakeMutableMessage(
        ctx, msg.arena, upb_FieldDef_MessageSubDef(def),
        upb_Message_Mutable(msg.value, def, msg.arena.ptr).msg);
  }
  AtomicField(ctx, def);
  return NULL;
}

static emacs_value SetField(emacs_env* env,
                            ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                            emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 3);
  struct MutableMessageArg msg = ExtractMutableMessage(ctx, args[0]);
  if (msg.type == NULL) return NULL;
  const upb_FieldDef* def = FindFieldBySymbol(ctx, msg.type, args[1]);
  if (def == NULL) return NULL;
  emacs_value value = args[2];
  SetFieldValue(ctx, msg.arena.ptr, msg.value, def, value);
  return value;
}

static emacs_value ClearField(emacs_env* env,
                              ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                              emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2);
  struct MutableMessageArg msg = ExtractMutableMessage(ctx, args[0]);
  if (msg.type == NULL) return NULL;
  const upb_FieldDef* def = FindFieldBySymbol(ctx, msg.type, args[1]);
  if (def == NULL) return NULL;
  upb_Message_ClearFieldByDef(msg.value, def);
  return Nil(ctx);
}

static emacs_value ArrayLength(emacs_env* env,
                               ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                               emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  struct ArrayArg array = ExtractArray(ctx, args[0]);
  if (array.type == NULL) return NULL;
  return MakeUInteger(ctx, upb_Array_Size(array.value));
}

static emacs_value ArrayElt(emacs_env* env,
                            ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                            emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2);
  struct ArrayArg array = ExtractArray(ctx, args[0]);
  if (array.type == NULL) return NULL;
  const upb_FieldDef* type = array.type;
  size_t index = ExtractIndex(ctx, array.value, args[0], args[1]);
  if (!Success(ctx)) return NULL;
  return MakeSingular(ctx, array.arena, type,
                      upb_Array_Get(array.value, index));
}

static emacs_value SetArrayElt(emacs_env* env,
                               ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                               emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 3);
  struct MutableArrayArg array = ExtractMutableArray(ctx, args[0]);
  if (array.type == NULL) return NULL;
  emacs_value elt = args[2];
  size_t index = ExtractIndex(ctx, array.value, args[0], args[1]);
  if (!Success(ctx)) return NULL;
  upb_MessageValue val = AdoptSingular(ctx, array.arena.ptr, array.type, elt);
  if (!Success(ctx)) return NULL;
  upb_Array_Set(array.value, index, val);
  return elt;
}

static emacs_value ArrayPop(emacs_env* env,
                            ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                            emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2);
  struct MutableArrayArg array = ExtractMutableArray(ctx, args[0]);
  if (array.type == NULL) return NULL;
  size_t index = ExtractIndex(ctx, array.value, args[0], args[1]);
  if (!Success(ctx)) return NULL;
  emacs_value elt = MakeSingular(ctx, array.arena, array.type,
                                 upb_Array_Get(array.value, index));
  if (!Success(ctx)) return NULL;
  upb_Array_Delete(array.value, index, 1);
  return elt;
}

static emacs_value DoArray(emacs_env* env,
                           ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                           emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2);
  emacs_value function = args[0];
  struct ArrayArg array = ExtractArray(ctx, args[1]);
  if (array.type == NULL) return NULL;
  const upb_FieldDef* type = array.type;
  const upb_Array* value = array.value;
  size_t size = upb_Array_Size(value);
  for (size_t i = 0; i < size; ++i) {
    emacs_value elt =
        MakeSingular(ctx, array.arena, type, upb_Array_Get(value, i));
    Funcall1(ctx, function, elt);
  }
  return Nil(ctx);
}

static emacs_value CopyArray(emacs_env* env,
                             ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                             emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  struct ArrayArg array = ExtractArray(ctx, args[0]);
  if (array.type == NULL) return NULL;
  const upb_FieldDef* type = array.type;
  const upb_Array* value = array.value;
  size_t size = upb_Array_Size(value);
  upb_Array* copy = NewArray(ctx, array.arena.ptr, type, size);
  if (copy == NULL) return NULL;
  for (size_t i = 0; i < size; ++i) {
    upb_Array_Set(copy, i, upb_Array_Get(array.value, i));
  }
  return MakeMutableArray(ctx, array.arena, type, copy);
}

static emacs_value Subarray(emacs_env* env, ptrdiff_t nargs, emacs_value* args,
                            void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2 || nargs == 3);
  struct ArrayArg array = ExtractArray(ctx, args[0]);
  if (array.type == NULL) return NULL;
  struct RangeArg range = ExtractRange(ctx, array.value, nargs, args);
  if (!range.ok) return NULL;
  size_t size = range.to - range.from;
  upb_Array* copy = NewArray(ctx, array.arena.ptr, array.type, size);
  if (copy == NULL) return NULL;
  for (size_t i = 0; i < size; ++i) {
    upb_Array_Set(copy, i, upb_Array_Get(array.value, range.from + i));
  }
  return MakeMutableArray(ctx, array.arena, array.type, copy);
}

static emacs_value MakeVectorFromArray(emacs_env* env,
                                       ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                                       emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  struct ArrayArg array = ExtractArray(ctx, args[0]);
  if (array.type == NULL) return NULL;
  size_t size = upb_Array_Size(array.value);
  if (size > PTRDIFF_MAX) {
    OverflowError1(ctx, MakeUInteger(ctx, size));
    return NULL;
  }
  ptrdiff_t ssize = (ptrdiff_t)size;
  emacs_value vector = MakeVector(ctx, ssize);
  for (ptrdiff_t i = 0; i < ssize; ++i) {
    upb_MessageValue val = upb_Array_Get(array.value, (size_t)i);
    SetVectorElement(ctx, vector, i,
                     MakeSingular(ctx, array.arena, array.type, val));
  }
  return vector;
}

static emacs_value ArrayDelete(emacs_env* env,
                               ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                               emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2 || nargs == 3);
  struct MutableArrayArg array = ExtractMutableArray(ctx, args[0]);
  if (array.type == NULL) return NULL;
  struct RangeArg range = ExtractRange(ctx, array.value, nargs, args);
  if (!range.ok) return NULL;
  upb_Array_Delete(array.value, range.from, range.to - range.from);
  return Nil(ctx);
}

static emacs_value ClearArray(emacs_env* env,
                              ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                              emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  struct MutableArrayArg array = ExtractMutableArray(ctx, args[0]);
  if (array.type == NULL) return NULL;
  ResizeArray(ctx, array.arena.ptr, array.value, 0);
  return Nil(ctx);
}

static emacs_value ReplaceArray(emacs_env* env,
                                ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                                emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2);
  struct MutableArrayArg dest = ExtractMutableArray(ctx, args[0]);
  if (dest.type == NULL) return NULL;
  const upb_Array* src = AdoptSequence(ctx, dest.arena.ptr, dest.type, args[1]);
  if (src == NULL) return NULL;
  size_t length = upb_Array_Size(src);
  if (!ResizeArray(ctx, dest.arena.ptr, dest.value, length)) return NULL;
  for (size_t i = 0; i < length; ++i) {
    upb_Array_Set(dest.value, i, upb_Array_Get(src, i));
  }
  return Nil(ctx);
}

static emacs_value ArrayMutableP(emacs_env* env,
                                 ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                                 emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  struct LispArray array = ExtractArrayStruct(ctx, args[0]);
  if (array.ptr == NULL) return NULL;
  return MakeBoolean(ctx, array.ptr->mutable);
}

static emacs_value PrintArray(emacs_env* env, ptrdiff_t nargs,
                              emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1 || nargs == 2);
  struct ArrayArg array = ExtractArray(ctx, args[0]);
  if (array.type == NULL) return NULL;
  emacs_value stream = OptionalArg(ctx, nargs, args, 1);
  size_t array_length = upb_Array_Size(array.value);
  size_t print_length = PrintLength(ctx);
  bool abbreviate = array_length > print_length;
  size_t limit = abbreviate ? print_length : array_length;
  PrincLiteral(ctx, "#<protocol buffer array with ", stream);
  Princ(ctx, MakeUInteger(ctx, array_length), stream);
  PrincLiteral(ctx, array_length == 1 ? " element [" : " elements [", stream);
  for (size_t i = 0; i < limit; ++i) {
    if (i > 0) PrincChar(ctx, ' ', stream);
    Prin1Singular(ctx, array.type, upb_Array_Get(array.value, i), stream);
  }
  PrincLiteral(ctx, abbreviate ? "...]>" : "]>", stream);
  return Nil(ctx);
}

static emacs_value AppendArray(emacs_env* env,
                               ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                               emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2);
  struct MutableArrayArg array = ExtractMutableArray(ctx, args[0]);
  if (array.type == NULL) return NULL;
  upb_MessageValue elt =
      AdoptSingular(ctx, array.arena.ptr, array.type, args[1]);
  if (!Success(ctx)) return NULL;
  if (!upb_Array_Append(array.value, elt, array.arena.ptr)) MemoryFull(ctx);
  return Nil(ctx);
}

static emacs_value ExtendArray(emacs_env* env,
                               ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                               emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2);
  struct MutableArrayArg dest = ExtractMutableArray(ctx, args[0]);
  if (dest.type == NULL) return NULL;
  const upb_Array* src = AdoptSequence(ctx, dest.arena.ptr, dest.type, args[1]);
  if (src == NULL) return NULL;
  size_t dest_size = upb_Array_Size(dest.value);
  size_t src_size = upb_Array_Size(src);
  size_t new_size;
  if (AddOverflowSize(ctx, dest_size, src_size, &new_size)) return NULL;
  if (!ResizeArray(ctx, dest.arena.ptr, dest.value, new_size)) return NULL;
  for (size_t i = 0; i < src_size; ++i) {
    upb_Array_Set(dest.value, dest_size + i, upb_Array_Get(src, i));
  }
  return Nil(ctx);
}

static emacs_value SortArray(emacs_env* env,
                             ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                             emacs_value* args, void* data) {
  struct Allocator alloc = HeapAllocator();
  struct Context ctx = {env, data};
  assert(nargs == 2);
  struct MutableArrayArg array = ExtractMutableArray(ctx, args[0]);
  if (array.type == NULL) return NULL;
  emacs_value pred = args[1];
  size_t size = upb_Array_Size(array.value);
  if (size < 2) return Nil(ctx);  // nothing to do
  // The qsort function doesn’t accept a context pointer, and qsort_r isn’t
  // portable.  Besides, both functions require the array to be contiguous,
  // which isn’t necessarily the case for upb_Array.  So we copy the necessary
  // context and element data into a new contiguous array.
  struct ArrayElement* elts = AllocateArray(ctx, alloc, size, sizeof *elts);
  if (elts == NULL) return NULL;
  struct SortContext sort_ctx = {ctx, pred, array.type, array.value};
  for (size_t i = 0; i < size; ++i) {
    upb_MessageValue val = upb_Array_Get(array.value, i);
    emacs_value lisp = MakeSingular(ctx, array.arena, array.type, val);
    if (!Success(ctx)) {
      Free(alloc, elts);
      return NULL;
    }
    struct ArrayElement elt = {&sort_ctx, val, lisp};
    elts[i] = elt;
  }
  qsort(elts, size, sizeof *elts, CompareArrayElements);
  // Copy back sorted elements into the original array.
  for (size_t i = 0; i < size; ++i) {
    upb_Array_Set(array.value, i, elts[i].val);
  }
  Free(alloc, elts);
  return Nil(ctx);
}

static emacs_value NreverseArray(emacs_env* env,
                                 ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                                 emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  struct MutableArrayArg array = ExtractMutableArray(ctx, args[0]);
  if (array.type == NULL) return NULL;
  size_t size = upb_Array_Size(array.value);
  for (size_t i = 0; i < size / 2; ++i) {
    size_t j = size - i - 1;
    upb_MessageValue u = upb_Array_Get(array.value, i);
    upb_MessageValue v = upb_Array_Get(array.value, j);
    upb_Array_Set(array.value, i, v);
    upb_Array_Set(array.value, j, u);
  }
  return Nil(ctx);
}

static emacs_value MapLength(emacs_env* env,
                             ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                             emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  struct MapArg map = ExtractMap(ctx, args[0]);
  if (map.type == NULL) return NULL;
  return MakeUInteger(ctx, upb_Map_Size(map.value));
}

static emacs_value MapContainsKey(emacs_env* env,
                                  ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                                  emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2);
  struct MapArg map = ExtractMap(ctx, args[0]);
  if (map.type == NULL) return NULL;
  struct MapType type = GetMapType(ctx, map.type);
  if (type.key == NULL) return NULL;
  upb_MessageValue key = AdoptScalar(ctx, map.arena.ptr, type.key, args[1]);
  if (!Success(ctx)) return NULL;
  return MakeBoolean(ctx, upb_Map_Get(map.value, key, NULL));
}

static emacs_value MapGet(emacs_env* env, ptrdiff_t nargs, emacs_value* args,
                          void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2 || nargs == 3);
  struct MapArg map = ExtractMap(ctx, args[0]);
  if (map.type == NULL) return NULL;
  struct MapType type = GetMapType(ctx, map.type);
  if (type.key == NULL) return NULL;
  upb_MessageValue key = AdoptScalar(ctx, map.arena.ptr, type.key, args[1]);
  if (!Success(ctx)) return NULL;
  upb_MessageValue value;
  bool ok = upb_Map_Get(map.value, key, &value);
  return ok ? MakeSingular(ctx, map.arena, type.value, value)
            : OptionalArg(ctx, nargs, args, 2);
}

static emacs_value MapPut(emacs_env* env, ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                          emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 3);
  struct MutableMapArg map = ExtractMutableMap(ctx, args[0]);
  if (map.type == NULL) return NULL;
  struct MapType type = GetMapType(ctx, map.type);
  if (type.key == NULL) return NULL;
  upb_MessageValue key = AdoptScalar(ctx, map.arena.ptr, type.key, args[1]);
  emacs_value ret = args[2];
  upb_MessageValue value = AdoptSingular(ctx, map.arena.ptr, type.value, ret);
  if (!Success(ctx)) return NULL;
  SetMapEntry(ctx, map.arena.ptr, map.value, key, value);
  return ret;
}

static emacs_value MapSet(emacs_env* env, ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                          emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 3);
  struct MutableMapArg map = ExtractMutableMap(ctx, args[0]);
  if (map.type == NULL) return NULL;
  struct MapType type = GetMapType(ctx, map.type);
  if (type.key == NULL) return NULL;
  upb_MessageValue key = AdoptScalar(ctx, map.arena.ptr, type.key, args[1]);
  upb_MessageValue value =
      AdoptSingular(ctx, map.arena.ptr, type.value, args[2]);
  if (!Success(ctx)) return NULL;
  upb_MapInsertStatus status =
      SetMapEntry(ctx, map.arena.ptr, map.value, key, value);
  return MakeBoolean(ctx, status == kUpb_MapInsertStatus_Inserted);
}

static emacs_value MapDelete(emacs_env* env,
                             ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                             emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2);
  struct MutableMapArg map = ExtractMutableMap(ctx, args[0]);
  if (map.type == NULL) return NULL;
  struct MapType type = GetMapType(ctx, map.type);
  if (type.key == NULL) return NULL;
  upb_MessageValue key = AdoptScalar(ctx, map.arena.ptr, type.key, args[1]);
  if (!Success(ctx)) return NULL;
  bool deleted = upb_Map_Delete(map.value, key, NULL);
  return MakeBoolean(ctx, deleted);
}

static emacs_value MapPop(emacs_env* env, ptrdiff_t nargs, emacs_value* args,
                          void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2 || nargs == 3);
  struct MutableMapArg map = ExtractMutableMap(ctx, args[0]);
  if (map.type == NULL) return NULL;
  struct MapType type = GetMapType(ctx, map.type);
  if (type.key == NULL) return NULL;
  upb_MessageValue key = AdoptScalar(ctx, map.arena.ptr, type.key, args[1]);
  if (!Success(ctx)) return NULL;
  upb_MessageValue val;
  bool deleted = upb_Map_Delete(map.value, key, &val);
  return deleted ? MakeSingular(ctx, map.arena, type.value, val)
                 : OptionalArg(ctx, nargs, args, 2);
}

static emacs_value ClearMap(emacs_env* env,
                            ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                            emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  struct MutableMapArg map = ExtractMutableMap(ctx, args[0]);
  if (map.type == NULL) return NULL;
  upb_Map_Clear(map.value);
  return Nil(ctx);
}

static emacs_value UpdateMap(emacs_env* env,
                             ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                             emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2);
  struct MutableMapArg dest = ExtractMutableMap(ctx, args[0]);
  if (dest.type == NULL) return NULL;
  const upb_Map* src = AdoptMap(ctx, dest.arena.ptr, dest.type, args[1]);
  if (src == NULL) return NULL;
  UpdateMapEntries(ctx, dest.arena.ptr, dest.value, src);
  return Nil(ctx);
}

static emacs_value ReplaceMap(emacs_env* env,
                              ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                              emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2);
  struct MutableMapArg dest = ExtractMutableMap(ctx, args[0]);
  if (dest.type == NULL) return NULL;
  const upb_Map* src = AdoptMap(ctx, dest.arena.ptr, dest.type, args[1]);
  if (src == NULL) return NULL;
  upb_Map_Clear(dest.value);
  UpdateMapEntries(ctx, dest.arena.ptr, dest.value, src);
  return Nil(ctx);
}

static emacs_value CopyMap(emacs_env* env,
                           ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                           emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  struct MapArg src = ExtractMap(ctx, args[0]);
  if (src.type == NULL) return NULL;
  struct MapType type = GetMapType(ctx, src.type);
  if (type.key == NULL) return NULL;
  upb_Map* dest = NewMap(ctx, src.arena.ptr, type);
  if (dest == NULL) return NULL;
  if (!UpdateMapEntries(ctx, src.arena.ptr, dest, src.value)) return NULL;
  return MakeMutableMap(ctx, src.arena, src.type, dest);
}

static emacs_value MapMutableP(emacs_env* env,
                               ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                               emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  struct LispMap map = ExtractMapStruct(ctx, args[0]);
  if (map.ptr == NULL) return NULL;
  return MakeBoolean(ctx, map.ptr->mutable);
}

static emacs_value PrintMap(emacs_env* env, ptrdiff_t nargs, emacs_value* args,
                            void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1 || nargs == 2);
  struct MapArg map = ExtractMap(ctx, args[0]);
  if (map.type == NULL) return NULL;
  struct MapType type = GetMapType(ctx, map.type);
  if (type.key == NULL) return NULL;
  emacs_value stream = OptionalArg(ctx, nargs, args, 1);
  size_t map_length = upb_Map_Size(map.value);
  size_t print_length = PrintLength(ctx);
  bool abbreviate = map_length > print_length;
  size_t limit = abbreviate ? print_length : map_length;
  PrincLiteral(ctx, "#<protocol buffer map with ", stream);
  Princ(ctx, MakeUInteger(ctx, map_length), stream);
  PrincLiteral(ctx, map_length == 1 ? " entry [" : " entries [", stream);
  size_t iter = kUpb_Map_Begin;
  upb_MessageValue key;
  upb_MessageValue value;
  size_t i = 0;
  while (i < limit && upb_Map_Next(map.value, &key, &value, &iter)) {
    if (i > 0) PrincChar(ctx, ' ', stream);
    PrincChar(ctx, '(', stream);
    Prin1Scalar(ctx, type.key, key, stream);
    PrincChar(ctx, ' ', stream);
    Prin1Singular(ctx, type.value, value, stream);
    PrincChar(ctx, ')', stream);
    ++i;
  }
  PrincLiteral(ctx, abbreviate ? "...]>" : "]>", stream);
  return Nil(ctx);
}

static emacs_value DoMap(emacs_env* env, ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                         emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2);
  emacs_value function = args[0];
  struct MapArg map = ExtractMap(ctx, args[1]);
  if (map.type == NULL) return NULL;
  struct MapType type = GetMapType(ctx, map.type);
  if (type.key == NULL) return NULL;
  const upb_Map* src = map.value;
  size_t iter = kUpb_Map_Begin;
  upb_MessageValue key;
  upb_MessageValue value;
  while (upb_Map_Next(src, &key, &value, &iter)) {
    Funcall2(ctx, function, MakeScalar(ctx, type.key, key),
             MakeSingular(ctx, map.arena, type.value, value));
  }
  return Nil(ctx);
}

static emacs_value Timestamp(emacs_env* env,
                             ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                             emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  struct MessageArg msg = ExtractWellKnownMessage(ctx, kUpb_WellKnown_Timestamp,
                                                  kTimestampP, args[0]);
  if (msg.type == NULL) return NULL;
  union {
    const upb_Message* msg;
    const google_protobuf_Timestamp* ts;
  } u = {msg.value};
  struct timespec time;
  time.tv_sec = google_protobuf_Timestamp_seconds(u.ts);
  time.tv_nsec = google_protobuf_Timestamp_nanos(u.ts);
  return MakeTime(ctx, time);
}

static emacs_value Duration(emacs_env* env,
                            ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                            emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  struct MessageArg msg = ExtractWellKnownMessage(ctx, kUpb_WellKnown_Duration,
                                                  kDurationP, args[0]);
  if (msg.type == NULL) return NULL;
  union {
    const upb_Message* msg;
    const google_protobuf_Duration* dur;
  } u = {msg.value};
  struct timespec time;
  time.tv_sec = google_protobuf_Duration_seconds(u.dur);
  time.tv_nsec = google_protobuf_Duration_nanos(u.dur);
  return MakeTime(ctx, time);
}

static emacs_value SetTimestamp(emacs_env* env,
                                ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                                emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2);
  struct MutableMessageArg msg = ExtractWellKnownMutableMessage(
      ctx, kUpb_WellKnown_Timestamp, kTimestampP, args[0]);
  if (msg.type == NULL) return NULL;
  union {
    upb_Message* msg;
    google_protobuf_Timestamp* ts;
  } u = {msg.value};
  emacs_value time = args[1];
  SetTimestampProto(ctx, u.ts, time);
  return time;
}

static emacs_value SetDuration(emacs_env* env,
                               ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                               emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2);
  struct MutableMessageArg msg = ExtractWellKnownMutableMessage(
      ctx, kUpb_WellKnown_Duration, kDurationP, args[0]);
  if (msg.type == NULL) return NULL;
  union {
    upb_Message* msg;
    google_protobuf_Duration* dur;
  } u = {msg.value};
  emacs_value time = args[1];
  SetDurationProto(ctx, u.dur, time);
  return time;
}

static emacs_value MakeTimestamp(emacs_env* env,
                                 ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                                 emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  struct LispArena arena = MakeArena(ctx);
  if (arena.ptr == NULL) return NULL;
  const upb_MessageDef* def =
      google_protobuf_Timestamp_getmsgdef(MutableDefPool(ctx));
  google_protobuf_Timestamp* msg = google_protobuf_Timestamp_new(arena.ptr);
  if (def == NULL || msg == NULL) {
    MemoryFull(ctx);
    return NULL;
  }
  if (!SetTimestampProto(ctx, msg, args[0])) return NULL;
  return MakeMessageStruct(ctx, arena, GlobalSymbol(ctx, kTimestampNew), def,
                           UPB_UPCAST(msg), true);
}

static emacs_value MakeDuration(emacs_env* env,
                                ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                                emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  struct LispArena arena = MakeArena(ctx);
  if (arena.ptr == NULL) return NULL;
  const upb_MessageDef* def =
      google_protobuf_Duration_getmsgdef(MutableDefPool(ctx));
  google_protobuf_Duration* msg = google_protobuf_Duration_new(arena.ptr);
  if (def == NULL || msg == NULL) {
    MemoryFull(ctx);
    return NULL;
  }
  if (!SetDurationProto(ctx, msg, args[0])) return NULL;
  return MakeMessageStruct(ctx, arena, GlobalSymbol(ctx, kDurationNew), def,
                           UPB_UPCAST(msg), true);
}

static emacs_value PackAny(emacs_env* env,
                           ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                           emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  struct MessageArg msg = ExtractMessage(ctx, args[0]);
  if (msg.type == NULL) return NULL;
  struct LispArena arena = MakeArena(ctx);
  if (arena.ptr == NULL) return NULL;
  const upb_MessageDef* any_def =
      google_protobuf_Any_getmsgdef(MutableDefPool(ctx));
  google_protobuf_Any* any = google_protobuf_Any_new(arena.ptr);
  if (any_def == NULL || any == NULL) {
    MemoryFull(ctx);
    return NULL;
  }
  struct MutableString type_url =
      TypeUrl(ctx, ArenaAllocator(arena.ptr), msg.type);
  struct MutableString serialized = SerializeMessage(
      ctx, arena.ptr, msg.type, msg.value,
      kUpb_EncodeOption_Deterministic | kUpb_EncodeOption_CheckRequired);
  if (type_url.data == NULL || serialized.data == NULL) return NULL;
  google_protobuf_Any_set_type_url(any, View(type_url));
  google_protobuf_Any_set_value(any, View(serialized));
  return MakeMutableMessage(ctx, arena, any_def, UPB_UPCAST(any));
}

static emacs_value UnpackAny(emacs_env* env,
                             ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                             emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  emacs_value arg = args[0];
  struct AnyArg any = ExtractAny(ctx, arg);
  if (any.value == NULL) return NULL;
  upb_StringView type_url = google_protobuf_Any_type_url(any.value);
  upb_StringView serialized = google_protobuf_Any_value(any.value);
  if (type_url.data == NULL || serialized.data == NULL) {
    UninitializedAny(ctx, arg);
    return NULL;
  }
  const upb_MessageDef* def = FindMessageByTypeUrl(ctx, type_url);
  if (def == NULL) return NULL;
  upb_Message* msg = ParseMessage(ctx, any.arena.ptr, def, serialized,
                                  kUpb_DecodeOption_CheckRequired);
  if (msg == NULL) return NULL;
  return MakeMutableMessage(ctx, any.arena, def, msg);
}

static emacs_value AnyTypeName(emacs_env* env,
                               ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                               emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  emacs_value arg = args[0];
  struct AnyArg any = ExtractAny(ctx, arg);
  if (any.value == NULL) return NULL;
  upb_StringView type_url = google_protobuf_Any_type_url(any.value);
  if (type_url.data == NULL) {
    UninitializedAny(ctx, arg);
    return NULL;
  }
  upb_StringView full_name = MessageNameFromTypeUrl(ctx, type_url);
  if (full_name.data == NULL) return NULL;
  return MakeString(ctx, full_name);
}

static emacs_value CheckRequired(emacs_env* env,
                                 ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                                 emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  struct MessageArg msg = ExtractMessage(ctx, args[0]);
  if (msg.type == NULL) return NULL;
  upb_FieldPathEntry* fields;
  if (upb_util_HasUnsetRequired(msg.value, msg.type, DefPool(ctx), &fields)) {
    assert(fields != NULL && fields->field != NULL);
    emacs_value paths = Nil(ctx);
    upb_FieldPathEntry* it = fields;
    while (it->field != NULL) {
      char buffer[0x1000];
      size_t length = upb_FieldPath_ToText(&it, buffer, sizeof buffer);
      if (length >= sizeof buffer) {
        // Mark very long field paths as truncated.
        memset(buffer + sizeof buffer - 3, '.', 3);
        length = sizeof buffer;
      }
      emacs_value path =
          MakeString(ctx, upb_StringView_FromDataAndSize(buffer, length));
      Push(ctx, path, &paths);
      assert(it != NULL);
    }
    free(fields);
    Signal2(ctx, kMissingRequiredField, MakeMessageName(ctx, msg.type),
            Nreverse(ctx, paths));
  }
  return Nil(ctx);
}

static emacs_value CheckFieldName(emacs_env* env,
                                  ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                                  emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2);
  const upb_MessageDef* def = FindMessageByStructName(ctx, args[0]);
  if (def == NULL) return NULL;
  FindFieldBySymbol(ctx, def, args[1]);
  return Nil(ctx);
}

static emacs_value CheckFieldKeyword(emacs_env* env,
                                     ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                                     emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 2);
  const upb_MessageDef* def = FindMessageByStructName(ctx, args[0]);
  if (def == NULL) return NULL;
  FindFieldByKeyword(ctx, def, args[1]);
  return Nil(ctx);
}

// We use OS-specific functions for InsertStdin and WriteStdout because Emacs
// closes the standard I/O streams on Windows.

static emacs_value InsertStdin(emacs_env* env,
                               ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                               emacs_value* args ABSL_ATTRIBUTE_UNUSED,
                               void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 0);
  FileHandle handle = StdinHandle(ctx);
  if (!ValidHandle(handle)) return NULL;
  char buffer[0x4000];
  upb_StringView result;
  while ((result = ReadHandle(ctx, handle, buffer, sizeof buffer)).data !=
         NULL) {
    FuncallSymbol1(ctx, kInsert, MakeUnibyteString(ctx, result));
  }
  return Nil(ctx);
}

static emacs_value WriteStdout(emacs_env* env,
                               ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED,
                               emacs_value* args, void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  struct Allocator alloc = HeapAllocator();
  FileHandle handle = StdoutHandle(ctx);
  if (!ValidHandle(handle)) return NULL;
  struct MutableString content = ExtractUnibyteString(ctx, alloc, args[0]);
  if (content.data == NULL) return NULL;
  upb_StringView left = View(content);
  while (left.size > 0 && WriteHandle(ctx, handle, &left));
  Free(alloc, content.data);
  return Nil(ctx);
}

static emacs_value ParseCodeGeneratorRequest(
    emacs_env* env, ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED, emacs_value* args,
    void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  upb_Arena* arena = NewArena(ctx);
  if (arena == NULL) return NULL;
  struct Allocator alloc = ArenaAllocator(arena);
  struct MutableString serialized = ExtractUnibyteString(ctx, alloc, args[0]);
  emacs_value ret = serialized.data == NULL
                        ? NULL
                        : ParseGeneratorRequest(ctx, arena, View(serialized));
  upb_Arena_Free(arena);
  return ret;
}

static emacs_value SerializeCodeGeneratorResponse(
    emacs_env* env, ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED, emacs_value* args,
    void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  upb_Arena* arena = NewArena(ctx);
  if (arena == NULL) return NULL;
  upb_StringView serialized = SerializeGeneratorResponse(ctx, arena, args[0]);
  emacs_value ret = MakeUnibyteString(ctx, serialized);
  upb_Arena_Free(arena);
  return ret;
}

static emacs_value RegisterFileDescriptor(
    emacs_env* env, ptrdiff_t nargs ABSL_ATTRIBUTE_UNUSED, emacs_value* args,
    void* data) {
  struct Context ctx = {env, data};
  assert(nargs == 1);
  emacs_value serialized = args[0];
  upb_Arena* arena = NewArena(ctx);
  if (arena == NULL) return NULL;
  const google_protobuf_FileDescriptorProto* file =
      ReadFileDescriptorProto(ctx, arena, serialized);
  if (file == NULL) {
    upb_Arena_Free(arena);
    return NULL;
  }
  RegisterFileDescriptorProto(ctx, file);
  upb_Arena_Free(arena);
  return Nil(ctx);
}

/// Module initialization

// Allocates a new Globals structure from the heap and initialize it.  Only the
// module initializer function should call this.
static const struct Globals* InitializeGlobals(emacs_env* env,
                                               upb_DefPool* pool) {
  struct Globals temp;
  temp.pool = pool;
#define X(enumerator, string)  \
  assert(IsAscii((string)));   \
  temp.symbols[(enumerator)] = \
      env->make_global_ref(env, env->intern(env, (string)));
  GLOBAL_SYMBOLS
#undef X
#define X(enumerator, string, message, parent) \
  assert(IsAscii(string));                     \
  temp.symbols[enumerator] =                   \
      env->make_global_ref(env, env->intern(env, "elisp/proto/" string));
  ERROR_SYMBOLS
#undef X
  // This symbol is not interned in the globals array because we use it only
  // once.
  emacs_value symbol = env->intern(env, "memory-signal-data");
  emacs_value cons = env->funcall(env, temp.symbols[kSymbolValue], 1, &symbol);
  emacs_value car = env->funcall(env, temp.symbols[kCar], 1, &cons);
  emacs_value cdr = env->funcall(env, temp.symbols[kCdr], 1, &cons);
  struct Cons data = {env->make_global_ref(env, car),
                      env->make_global_ref(env, cdr)};
  temp.memory_signal_data = data;
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) return NULL;
  struct Globals* globals = malloc(sizeof *globals);
  if (globals == NULL) return NULL;
  *globals = temp;
  return globals;
}

static void DefineErrors(struct Context ctx) {
#define X(enumerator, string, message, parent) \
  DefineError(ctx, enumerator, message, parent);
  ERROR_SYMBOLS
#undef X
}

enum InitializationResult {
  kSuccess,
  kRuntimeTooSmall,
  kEmacsTooOld,
  kCannotAllocateDefPool,
  kCannotInitializeGlobals,
  kCannotRegisterKnownTypes
};

// Make the module work even if we compile with -fvisibility=hidden.
#if ABSL_HAVE_ATTRIBUTE(visibility)
#  define VISIBLE __attribute__((visibility("default")))
#else
#  define VISIBLE
#endif

int VISIBLE emacs_module_init(struct emacs_runtime* rt) {
  enum {
    kMinimumRuntimeSize = sizeof *rt,
    kMinimumEnvironmentSize = sizeof(struct emacs_env_29)
  };
  if (rt->size < kMinimumRuntimeSize) return kRuntimeTooSmall;
  emacs_env* env = rt->get_environment(rt);
  if (env->size < kMinimumEnvironmentSize) return kEmacsTooOld;
  upb_DefPool* pool = upb_DefPool_New();
  if (pool == NULL) return kCannotAllocateDefPool;
  // We have to register all message types that we use with and without
  // reflection eagerly, otherwise the layouts used for the two methods might
  // differ.
  if (google_protobuf_Any_getmsgdef(pool) == NULL ||
      google_protobuf_Duration_getmsgdef(pool) == NULL ||
      google_protobuf_Timestamp_getmsgdef(pool) == NULL) {
    return kCannotRegisterKnownTypes;
  }
  const struct Globals* globals = InitializeGlobals(env, pool);
  if (globals == NULL) return kCannotInitializeGlobals;
  struct Context ctx = {env, globals};
  Defun(ctx, "elisp/proto/make", 1, emacs_variadic_function,
        "Create a new mutable protocol buffer message object.\n"
        "TYPE must be a symbol denoting a generated protocol buffer\n"
        "structure type.  KEYS are keyword-value pairs to initialize\n"
        "the fields with.\n\n"
        "(fn type &rest keys)",
        Params3(kSymbol, kAndRest, kT), kMessage, kNoSideEffects, Make);
  Defun(ctx, "elisp/proto/parse", 2, 4,
        "Parse a serialized protocol buffer message.\n"
        "TYPE must be a symbol denoting a generated protocol buffer\n"
        "structure type.  SERIALIZED must be a unibyte string containing the\n"
        "binary serialization of a protocol buffer message of the given\n"
        "TYPE.  Return a mutable message object of the given TYPE.\n"
        "If ALLOW-PARTIAL is non-nil, don’t check whether required fields\n"
        "are present.\n\n"
        "(fn type serialized &key allow-partial)",
        Params4(kSymbol, kUnibyteString, kAndRest, kT), kMessage,
        kNoSideEffects, Parse);
  Defun(ctx, "elisp/proto/parse-json", 2, 4,
        "Parse a protocol buffer message from its JSON representation.\n"
        "TYPE must be a symbol denoting a generated protocol buffer\n"
        "structure type.  JSON must be a string containing the\n"
        "JSON serialization of a protocol buffer message of the given\n"
        "TYPE.  Return a mutable message object of the given TYPE.\n"
        "If DISCARD-UNKNOWN is non-nil, ignore unknown fields.\n\n"
        "(fn type json &key discard-unknown)",
        Params4(kSymbol, kString, kAndRest, kT), kMessage, kNoSideEffects,
        ParseJson);
  Defun(ctx, "elisp/proto/serialize", 1, 7,
        "Serialize a protocol buffer MESSAGE to its binary form.\n"
        "The return value is a unibyte string.\n"
        "If ALLOW-PARTIAL is non-nil, don’t check whether required fields\n"
        "are present.  If DISCARD-UNKNOWN is non-nil, skip unknown fields.\n"
        "If DETERMINISTIC is non-nil, attempt to produce more deterministic\n"
        "output.\n\n"
        "(fn message &key allow-partial discard-unknown deterministic)",
        Params3(kMessage, kAndRest, kT), kUnibyteString, kNoSideEffects,
        Serialize);
  Defun(ctx, "elisp/proto/serialize-text", 1, 7,
        "Serialize a protocol buffer MESSAGE to its text representation.\n"
        "If COMPACT is non-nil, use a more compact representation.\n"
        "If DISCARD-UNKNOWN is non-nil, skip unknown fields.\n"
        "If DETERMINISTIC is non-nil, attempt to produce more deterministic\n"
        "output.\n\n"
        "(fn message &key compact discard-unknown deterministic)",
        Params3(kMessage, kAndRest, kT), kString, kNoSideEffects,
        SerializeText);
  Defun(ctx, "elisp/proto/serialize-json", 1, 5,
        "Serialize a protocol buffer MESSAGE to its JSON representation.\n"
        "If EMIT-DEFAULTS is non-nil, emit default values.\n"
        "If PROTO-NAMES is non-nil, use protocol buffer (snake-case)\n"
        "instead of JSON (camel-case) names.\n\n"
        "(fn message &key emit-defaults proto-names)",
        Params3(kMessage, kAndRest, kT), kString, kNoSideEffects, SerializeJson);
  Defun(ctx, "elisp/proto/message-mutable-p", 1, 1,
        "Return whether the protocol buffer MESSAGE is mutable.\n"
        "Signal an error if MESSAGE is not a protocol buffer message.\n\n"
        "(fn message)",
        Params1(kMessage), kT, kNoSideEffects, MessageMutableP);
  Defun(ctx, "elisp/proto/print-message", 1, 2,
        "Print protocol buffer MESSAGE to STREAM.\n"
        "MESSAGE must be a protocol buffer message object, i.e., its type\n"
        "must be a subtype of ‘elisp/proto/message’.\n"
        "STREAM must be an output stream as defined\n"
        "in the Info node ‘(elisp) Output Streams’.\n\n"
        "(fn message &optional stream)",
        Params3(kMessage, kAndOptional, kT), kT, 0, PrintMessage);
  Defun(ctx, "elisp/proto/clear-message", 1, 1,
        "Clear all fields in MESSAGE.\n"
        "MESSAGE must be a mutable protocol buffer message object, i.e.,\n"
        "its type must be a subtype of ‘elisp/proto/message’.\n\n"
        "(fn message)",
        Params1(kMessage), kT, 0, ClearMessage);
  Defun(ctx, "elisp/proto/has-field", 2, 2,
        "Return non-nil if the FIELD is present in MESSAGE.\n"
        "MESSAGE must be a protocol buffer message object, i.e., its type\n"
        "must be a subtype of ‘elisp/proto/message’.  FIELD must be a symbol\n"
        "denoting a valid field in MESSAGE.  Signal an error if the field\n"
        "has no notion of presence, e.g. for repeated fields.\n\n"
        "(fn message field)",
        Params2(kMessage, kSymbol), kT, kNoSideEffects, HasField);
  Defun(ctx, "elisp/proto/field", 2, 2,
        "Return the value of the FIELD in the MESSAGE.\n"
        "MESSAGE must be a protocol buffer message object, i.e., its type\n"
        "must be a subtype of ‘elisp/proto/message’.  FIELD must be a symbol\n"
        "denoting a valid field in MESSAGE.  Return nil if FIELD denotes a\n"
        "non-scalar field that is not present in MESSAGE.\n\n"
        "(fn message field)",
        Params2(kMessage, kSymbol), kT, kNoSideEffects, Field);
  Defun(ctx, "elisp/proto/mutable-field", 2, 2,
        "Return the mutable value of the FIELD in the MESSAGE.\n"
        "MESSAGE must be a mutable protocol buffer message object, i.e.,\n"
        "its type must be a subtype of ‘elisp/proto/message’.\n"
        "FIELD must be a symbol denoting a valid field in MESSAGE.\n"
        "If FIELD denotes a non-scalar field that is not present in MESSAGE,\n"
        "create the field and return a mutable value for it.\n\n"
        "(fn message field)",
        Params2(kMessage, kSymbol), kT, 0, MutableField);
  Defun(ctx, "elisp/proto/set-field", 3, 3,
        "Set the FIELD in the MESSAGE to VALUE and return VALUE.\n"
        "MESSAGE must be a mutable protocol buffer message object, i.e.,\n"
        "its type must be a subtype of ‘elisp/proto/message’.\n"
        "FIELD must be a symbol denoting a valid field in MESSAGE.\n\n"
        "(fn message field value)",
        Params3(kMessage, kSymbol, kT), kT, 0, SetField);
  Defun(ctx, "elisp/proto/clear-field", 2, 2,
        "Clear the FIELD in the MESSAGE.\n"
        "MESSAGE must be a mutable protocol buffer message object, i.e.,\n"
        "its type must be a subtype of ‘elisp/proto/message’.\n"
        "FIELD must be a symbol denoting a valid field in MESSAGE.\n\n"
        "(fn message field)",
        Params2(kMessage, kSymbol), kT, 0, ClearField);
  Defun(ctx, "elisp/proto/array-mutable-p", 1, 1,
        "Return whether the protocol buffer ARRAY is mutable.\n"
        "Signal an error if ARRAY is not a protocol buffer array.\n\n"
        "(fn array)",
        Params1(kArray), kT, kNoSideEffects, ArrayMutableP);
  Defun(ctx, "elisp/proto/print-array", 1, 2,
        "Print protocol buffer ARRAY to STREAM.\n"
        "ARRAY must be a protocol buffer array of type ‘elisp/proto/array’.\n"
        "STREAM must be an output stream as defined\n"
        "in the Info node ‘(elisp) Output Streams’.\n\n"
        "(fn message &optional stream)",
        Params3(kMessage, kAndOptional, kT), kT, 0, PrintArray);
  Defun(ctx, "elisp/proto/array-length", 1, 1,
        "Return the number of elements in the protocol buffer ARRAY.\n"
        "ARRAY must be a protocol buffer array of type ‘elisp/proto/array’.\n\n"
        "(fn array)",
        Params1(kArray), kInteger, kNoSideEffects, ArrayLength);
  Defun(ctx, "elisp/proto/array-elt", 2, 2,
        "Return the element at INDEX in the protocol buffer ARRAY.\n"
        "ARRAY must be a protocol buffer array of type ‘elisp/proto/array’.\n\n"
        "(fn array index)",
        Params2(kArray, kInteger), kT, kNoSideEffects, ArrayElt);
  Defun(ctx, "elisp/proto/set-array-elt", 3, 3,
        "Set the element at INDEX in the protocol buffer ARRAY to VALUE.\n"
        "ARRAY must be a mutable protocol buffer array of type\n"
        "‘elisp/proto/array’.  Return VALUE.\n\n"
        "(fn array index value)",
        Params3(kArray, kInteger, kT), kT, 0, SetArrayElt);
  Defun(ctx, "elisp/proto/array-pop", 2, 2,
        "Remove and return the element at INDEX in the protocol buffer ARRAY.\n"
        "ARRAY must be a mutable protocol buffer array of type\n"
        "‘elisp/proto/array’.\n\n"
        "(fn array index)",
        Params2(kArray, kInteger), kT, 0, ArrayPop);
  Defun(ctx, "elisp/proto/array-delete", 2, 3,
        "Delete an element range from the protocol buffer ARRAY.\n"
        "ARRAY must be a mutable protocol buffer array of type\n"
        "‘elisp/proto/array’.  The index range includes FROM,\n"
        "but excludes TO.  If TO is not given or nil, delete everything\n"
        "starting at FROM until the end of ARRAY.  FROM and TO can be\n"
        "negative, in which case they count from the end of ARRAY.\n"
        "The remaining elements after TO are shifted to the left.\n\n"
        "(fn array from &optional to)",
        Params4(kArray, kInteger, kAndOptional, kT), kT, 0, ArrayDelete);
  Defun(ctx, "elisp/proto/do-array", 2, 2,
        "Call FUNCTION for each element in ARRAY.\n"
        "ARRAY must be a protocol buffer array of type ‘elisp/proto/array’.\n\n"
        "(fn function array)",
        Params2(kFunction, kArray), kT, 0, DoArray);
  Defun(ctx, "elisp/proto/copy-array", 1, 1,
        "Return a shallow copy of ARRAY.\n"
        "ARRAY must be a protocol buffer array of type ‘elisp/proto/array’.\n"
        "The return value is mutable.\n\n"
        "(fn array)",
        Params1(kArray), kArray, kNoSideEffects, CopyArray);
  Defun(ctx, "elisp/proto/subarray", 2, 3,
        "Return a shallow copy of a subarray of ARRAY.\n"
        "ARRAY must be a protocol buffer array of type\n"
        "‘elisp/proto/array’.  The index range includes FROM,\n"
        "but excludes TO.  If TO is not given or nil, use the subarray\n"
        "starting at FROM until the end of ARRAY.  FROM and TO can be\n"
        "negative, in which case they count from the end of ARRAY.\n"
        "The return value is mutable.\n\n"
        "(fn array from &optional to)",
        Params4(kArray, kInteger, kAndOptional, kT), kArray,
        kNoSideEffects, Subarray);
  Defun(ctx, "elisp/proto/make-vector-from-array", 1, 1,
        "Return a shallow copy of ARRAY as a vector.\n"
        "ARRAY must be a protocol buffer array of type ‘elisp/proto/array’.\n\n"
        "(fn array)",
        Params1(kArray), kVector, kNoSideEffects, MakeVectorFromArray);
  Defun(ctx, "elisp/proto/clear-array", 1, 1,
        "Remove all elements from the protocol buffer ARRAY.\n"
        "ARRAY must be a mutable protocol buffer array of type\n"
        "‘elisp/proto/array’.\n"
        "After this function returns, ARRAY is empty.\n\n"
        "(fn array)",
        Params1(kArray), kT, 0, ClearArray);
  Defun(ctx, "elisp/proto/replace-array", 2, 2,
        "Replace the elements from the protocol buffer DEST with SOURCE.\n"
        "DEST must be a mutable protocol buffer array of type\n"
        "‘elisp/proto/array’ and SOURCE must be a generalized sequence.\n"
        "Clear DEST and then copy all elements from SOURCE into DEST.\n\n"
        "(fn dest source)",
        Params2(kArray, kT), kT, 0, ReplaceArray);
  Defun(ctx, "elisp/proto/append-array", 2, 2,
        "Append VALUE to the protocol buffer ARRAY.\n"
        "ARRAY must be a mutable protocol buffer array of type\n"
        "‘elisp/proto/array’.\n\n"
        "(fn array value)",
        Params2(kArray, kT), kT, 0, AppendArray);
  Defun(ctx, "elisp/proto/extend-array", 2, 2,
        "Append SOURCE to the protocol buffer array DEST.\n"
        "DEST must be a mutable protocol buffer array of type\n"
        "‘elisp/proto/array’ and SOURCE must be a generalized sequence.\n\n"
        "(fn dest source)",
        Params2(kArray, kT), kT, 0, ExtendArray);
  Defun(ctx, "elisp/proto/sort-array", 2, 2,
        "Sort the protocol buffer ARRAY in place, comparing values with PRED.\n"
        "ARRAY must be a mutable protocol buffer array of type\n"
        "‘elisp/proto/array’.\n\n"
        "(fn array pred)",
        Params2(kArray, kFunction), kT, 0, SortArray);
  Defun(ctx, "elisp/proto/nreverse-array", 1, 1,
        "Reverse the protocol buffer ARRAY in place.\n"
        "ARRAY must be a mutable protocol buffer array of type\n"
        "‘elisp/proto/array’.\n\n"
        "(fn array)",
        Params1(kArray), kT, 0, NreverseArray);
  Defun(ctx, "elisp/proto/map-mutable-p", 1, 1,
        "Return whether the protocol buffer MAP is mutable.\n"
        "Signal an error if MAP is not a protocol buffer map.\n\n"
        "(fn map)",
        Params1(kMap), kT, kNoSideEffects, MapMutableP);
  Defun(ctx, "elisp/proto/print-map", 1, 2,
        "Print protocol buffer MAP to STREAM.\n"
        "MAP must be a protocol buffer map of type ‘elisp/proto/map’.\n"
        "STREAM must be an output stream as defined\n"
        "in the Info node ‘(elisp) Output Streams’.\n\n"
        "(fn map &optional stream)",
        Params3(kMap, kAndOptional, kT), kT, 0, PrintMap);
  Defun(ctx, "elisp/proto/map-length", 1, 1,
        "Return the number of elements in the protocol buffer MAP.\n"
        "MAP must be a protocol buffer map of type ‘elisp/proto/map’.\n\n"
        "(fn map)",
        Params1(kMap), kInteger, kNoSideEffects, MapLength);
  Defun(ctx, "elisp/proto/map-contains-key", 2, 2,
        "Return whether the protocol buffer MAP contains KEY.\n"
        "MAP must be a protocol buffer map of type ‘elisp/proto/map’.\n\n"
        "(fn map key)",
        Params2(kMap, kT), kT, kNoSideEffects, MapContainsKey);
  Defun(ctx, "elisp/proto/map-get", 2, 3,
        "Return the value mapped to KEY in the protocol buffer MAP.\n"
        "MAP must be a protocol buffer map of type ‘elisp/proto/map’.\n"
        "Return DEFAULT if KEY isn’t present in MAP.\n\n"
        "(fn map key &optional default)",
        Params4(kMap, kT, kAndOptional, kT), kT, kNoSideEffects, MapGet);
  Defun(ctx, "elisp/proto/map-put", 3, 3,
        "Insert a VALUE with a KEY into the protocol buffer MAP.\n"
        "MAP must be a mutable protocol buffer map of type ‘elisp/proto/map’.\n"
        "If an entry with KEY is already present in MAP, overwrite it.\n"
        "Return VALUE.\n\n"
        "(fn map key value)",
        Params3(kMap, kT, kT), kT, 0, MapPut);
  Defun(ctx, "elisp/proto/map-set", 3, 3,
        "Insert a VALUE with a KEY into the protocol buffer MAP.\n"
        "MAP must be a mutable protocol buffer map of type ‘elisp/proto/map’.\n"
        "If an entry with KEY is already present in MAP, overwrite it\n"
        "and return nil; otherwise insert a new entry and return\n"
        "a non-nil value.\n\n"
        "(fn map key value)",
        Params3(kMap, kT, kT), kT, 0, MapSet);
  Defun(ctx, "elisp/proto/map-delete", 2, 2,
        "Attempt to remove KEY from the protocol buffer MAP.\n"
        "MAP must be a mutable protocol buffer map of type ‘elisp/proto/map’.\n"
        "Return whether KEY has been removed.\n\n"
        "(fn map key)",
        Params2(kMap, kT), kT, 0, MapDelete);
  Defun(ctx, "elisp/proto/map-pop", 2, 3,
        "Remove the KEY from the protocol buffer MAP and return its value.\n"
        "MAP must be a mutable protocol buffer map of type ‘elisp/proto/map’.\n"
        "If an entry with KEY is present in MAP, remove the entry and return\n"
        "its value.  Otherwise, don’t modify MAP and return DEFAULT.\n\n"
        "(fn map key &optional default)",
        Params4(kMap, kT, kAndOptional, kT), kT, 0, MapPop);
  Defun(ctx, "elisp/proto/clear-map", 1, 1,
        "Remove all entries from the protocol buffer MAP.\n"
        "MAP must be a mutable protocol buffer map of type ‘elisp/proto/map’.\n"
        "After this function returns, MAP is empty.\n\n"
        "(fn map)",
        Params1(kMap), kT, 0, ClearMap);
  Defun(ctx, "elisp/proto/update-map", 2, 2,
        "Update the protocol buffer map DEST from the given SOURCE.\n"
        "DEST must be a mutable protocol buffer map of type ‘elisp/proto/map’\n"
        "and SOURCE must be a generalized map type.  Copy all entries from\n"
        "SOURCE into DEST.  If a key from SOURCE is already present in DEST,\n"
        "overwrite it.\n\n"
        "(fn dest source)",
        Params2(kMap, kT), kT, 0, UpdateMap);
  Defun(ctx, "elisp/proto/replace-map", 2, 2,
        "Replace the entries from the protocol buffer DEST with SOURCE.\n"
        "DEST must be a mutable protocol buffer map of type ‘elisp/proto/map’\n"
        "and SOURCE must be a generalized map.  Clear DEST and then copy\n"
        "all entries from SOURCE into DEST.\n\n"
        "(fn DEST SOURCE)",
        Params2(kMap, kT), kT, 0, ReplaceMap);
  Defun(ctx, "elisp/proto/copy-map", 1, 1,
        "Return a shallow copy of MAP.\n"
        "MAP must be a protocol buffer map of type ‘elisp/proto/map’.\n"
        "The return value is mutable.\n\n"
        "(fn map)",
        Params1(kMap), kMap, kNoSideEffects, CopyMap);
  Defun(ctx, "elisp/proto/do-map", 2, 2,
        "Call FUNCTION for each element in MAP.\n"
        "MAP must be a protocol buffer array of type ‘elisp/proto/map’.\n"
        "Call FUNCTION with two arguments, the key and the value.\n\n"
        "(fn function map)",
        Params2(kFunction, kMap), kT, 0, DoMap);
  Defun(ctx, "elisp/proto/timestamp", 1, 1,
        "Return a time value stored in a protocol buffer TIMESTAMP.\n"
        "TIMESTAMP must be a google.protobuf.Timestamp message.\n\n"
        "(fn timestamp)",
        Params1(kTimestamp), kT, kNoSideEffects, Timestamp);
  Defun(ctx, "elisp/proto/duration", 1, 1,
        "Return a time value stored in a protocol buffer DURATION.\n"
        "DURATION must be a google.protobuf.Duration message.\n\n"
        "(fn duration)",
        Params1(kDuration), kT, kNoSideEffects, Duration);
  Defun(ctx, "elisp/proto/set-timestamp", 2, 2,
        "Set the protocol buffer TIMESTAMP to the given TIME.\n"
        "TIMESTAMP must be a mutable google.protobuf.Timestamp message.\n"
        "Return TIME.\n\n"
        "(fn timestamp time)",
        Params2(kTimestamp, kT), kT, 0, SetTimestamp);
  Defun(ctx, "elisp/proto/set-duration", 2, 2,
        "Set the protocol buffer DURATION to the given TIME.\n"
        "DURATION must be a mutable google.protobuf.Duration message.\n"
        "Return TIME.\n\n"
        "(fn duration time)",
        Params2(kMessage, kT), kT, 0, SetDuration);
  Defun(ctx, "elisp/proto/make-timestamp", 1, 1,
        "Return a google.protobuf.Timestamp message with the given TIME.\n\n"
        "(fn time)",
        Params1(kT), kTimestamp, kNoSideEffects, MakeTimestamp);
  Defun(ctx, "elisp/proto/make-duration", 1, 1,
        "Return a google.protobuf.Duration message with the given TIME.\n\n"
        "(fn time)",
        Params1(kT), kDuration, kNoSideEffects, MakeDuration);
  Defun(ctx, "elisp/proto/pack-any", 1, 1,
        "Return a new mutable google.protobuf.Any message wrapping MESSAGE.\n\n"
        "(fn message)",
        Params1(kMessage), kAny, kNoSideEffects, PackAny);
  Defun(ctx, "elisp/proto/unpack-any", 1, 1,
        "Unpack the message wrapped in ANY.\n"
        "ANY must be a google.protobuf.Any message.\n\n"
        "(fn any)",
        Params1(kAny), kMessage, kNoSideEffects, UnpackAny);
  Defun(ctx, "elisp/proto/any-type-name", 1, 1,
        "Return the type name of the messaged packed inside ANY.\n"
        "The return value is the full name of the message type.\n"
        "ANY must be a google.protobuf.Any message.\n\n"
        "(fn any)",
        Params1(kAny), kString, kNoSideEffects, AnyTypeName);
  // The check functions are technically side-effect-free, but their return
  // value is never used, so don’t mark them as such to prevent the byte
  // compiler from complaining.
  Defun(ctx, "elisp/proto/check-required", 1, 1,
        "Check that all required fields in MESSAGE are set.\n"
        "If a required field is not set, signal an error of type\n"
        "‘elisp/proto/missing-required-field’.\n\n"
        "(fn message)",
        Params1(kMessage), kT, 0, CheckRequired);
  Defun(ctx, "elisp/proto/check-field-name", 2, 2,
        "Check if FIELD is a field in the protocol buffer message TYPE.\n"
        "Signal an error of type ‘elisp/proto/unknown-field’ if not.\n\n"
        "(fn type field)",
        Params2(kSymbol, kSymbol), kT, 0, CheckFieldName);
  Defun(ctx, "elisp/proto/check-field-keyword", 2, 2,
        "Check if KEYWORD refers a field in the protocol buffer message TYPE.\n"
        "Signal an error of type ‘elisp/proto/unknown-field’ if not.\n\n"
        "(fn type keyword)",
        Params2(kSymbol, kSymbol), kT, 0, CheckFieldKeyword);
  Defun(ctx, "elisp/proto/insert-stdin", 0, 0,
        "Insert standard input into current buffer.\n"
        "This function is used by the protocol buffer compiler;\n"
        "users should not call it directly.",
        Params0(), kT, 0, InsertStdin);
  Defun(ctx, "elisp/proto/write-stdout", 1, 1,
        "Write STRING to standard output.\n"
        "STRING must be a unibyte string.\n"
        "This function is used by the protocol buffer compiler;\n"
        "users should not call it directly.\n\n"
        "(fn string)",
        Params1(kString), kT, 0, WriteStdout);
  Defun(ctx, "elisp/proto/parse-code-generator-request", 1, 1,
        "Parse a protocol buffer code generator request.\n"
        "SERIALIZED must be the serialized form of a\n"
        "google.protobuf.compiler.CodeGeneratorRequest message.\n"
        "Return a nested list\n"
        "((PROTO-FILE-NAME SERIALIZED-FILE-DESCRIPTOR-PROTO\n"
        "  (DEPENDENCY-FILE-NAME...)\n"
        "  ((MESSAGE-FULL-NAME FIELD-NAME...)...)\n"
        "  ((ENUM-FULL-NAME (NAME VALUE)...)...))...).\n"
        "This function is used by the protocol buffer compiler;\n"
        "users should not call it directly.\n\n"
        "(fn serialized)",
        Params1(kString), kList, kNoSideEffects, ParseCodeGeneratorRequest);
  Defun(ctx, "elisp/proto/serialize-code-generator-response", 1, 1,
        "Generate and serialize a protocol buffer generator response.\n"
        "FILES must be a list of (NAME . CONTENT) pairs,\n"
        "where NAME is a string specifying the name of the file to generate,\n"
        "and CONTENT are the corresponding file contents.\n"
        "Convert FILES to a message of type\n"
        "google.protobuf.compiler.CodeGeneratorResponse\n"
        "and return its serialized form as a unibyte string.\n"
        "This function is used by the protocol buffer compiler;\n"
        "users should not call it directly.\n\n"
        "(fn files)",
        Params1(kList), kString, kNoSideEffects,
        SerializeCodeGeneratorResponse);
  Defun(ctx, "elisp/proto/register-file-descriptor", 1, 1,
        "Register a protocol buffer file descriptor set.\n"
        "SERIALIZED must be the serialized form of a\n"
        "google.protobuf.FileDescriptorProto message.\n"
        "This function is used by the protocol buffer compiler;\n"
        "users should not call it directly.\n\n"
        "(fn serialized)",
        Params1(kString), kT, 0, RegisterFileDescriptor);
  DefineErrors(ctx);
  Provide(ctx, "elisp/proto/module");
  return kSuccess;
}

int VISIBLE plugin_is_GPL_compatible = 1;

#undef VISIBLE
