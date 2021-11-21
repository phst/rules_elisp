# Copyright 2021 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

SHELL := /bin/sh

.DEFAULT: all
.SUFFIXES:

BAZEL := bazel
BAZELFLAGS :=
FIND := find
GREP := grep
CP := cp

# All potentially supported Emacs versions.
versions := 26.1 26.2 26.3 27.1 27.2

pytype_target := pytype

kernel := $(shell uname -s)
ifeq ($(kernel),Linux)
  # GNU/Linux supports all Emacs versions.
else ifeq ($(kernel),Darwin)
  # macOS only supports Emacs 27.
  unsupported := 26.1 26.2 26.3
  ifneq ($(shell uname -m),x86_64)
    # Apple Silicon doesn’t support Emacs 27.1.
    unsupported += 27.1
  endif
else ifeq ($(kernel:MINGW64_NT-%=mingw64),mingw64)
  # Windows only supports Emacs 27.
  unsupported := 26.1 26.2 26.3
  BAZELFLAGS += --compiler=mingw-gcc
  pytype_target :=
  # Don’t munge Bazel target labels.  See
  # https://www.msys2.org/docs/filesystem-paths/#process-arguments.
  export MSYS2_ARG_CONV_EXCL := //;--extra_toolchains
else
  $(error Unsupported kernel $(kernel))
endif

versions := $(filter-out $(unsupported),$(versions))

# Test both default toolchain and versioned toolchains.
all: buildifier pylint $(pytype_target) nogo docs check $(versions)

buildifier:
	$(BAZEL) run $(BAZELFLAGS) -- \
	  @com_github_bazelbuild_buildtools//buildifier \
	  --mode=check --lint=warn -r -- "$${PWD}"

pylint:
	$(BAZEL) run $(BAZELFLAGS) -- //:run_pylint

pytype:
	$(BAZEL) run $(BAZELFLAGS) -- //:run_pytype

# We don’t want any Go rules in the public packages, as our users would have to
# depend on the Go rules then as well.
nogo:
	echo 'Looking for unwanted Go targets in public packages'
	! $(FIND) elisp emacs -type f \
	  -exec $(GREP) -F -e '@io_bazel_rules_go' -n -- '{}' '+' \
	  || { echo 'Unwanted Go targets found'; exit 1; }

check:
	$(BAZEL) test --test_output=errors $(BAZELFLAGS) -- //...

$(versions):
	$(MAKE) check BAZELFLAGS='$(BAZELFLAGS) --extra_toolchains=//elisp:emacs_$@_toolchain'

doc_targets := $(shell $(BAZEL) query --output=label 'filter("\.md\.generated$$", kind("generated file", //...:*))')
doc_generated := $(addprefix bazel-bin/,$(subst :,/,$(doc_targets://%=%)))
doc_sources := $(doc_generated:bazel-bin/%.generated=%)

docs: $(doc_sources)

$(doc_sources): %: bazel-bin/%.generated
	$(CP) -- '$<' '$@'

$(doc_generated) &:
	$(BAZEL) build $(BAZELFLAGS) -- $(doc_targets)

.PHONY: all buildifier pylint pytype nogo check $(versions)
.PHONY: docs $(doc_sources) $(doc_generated)
