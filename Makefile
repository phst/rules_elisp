# Copyright 2021, 2023-2026 Google LLC
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

.POSIX:
.SUFFIXES:

SHELL = /bin/sh

BAZEL = bazel
BAZELFLAGS =
GIT = git
EMACS = emacs
EMACSFLAGS =
SHELLCHECK = shellcheck
SHELLCHECKFLAGS =
HYPERFINE = hyperfine
HYPERFINEFLAGS =

all:
	$(BAZEL) build $(BAZELFLAGS) -- //...

generate: compdb coverage

check: all check-extra
	./check.ps1

GENERATE_BAZELFLAGS = $(BAZELFLAGS) --lockfile_mode=off
COMPDB_BAZELFLAGS = $(GENERATE_BAZELFLAGS) --norun_validations \
  --output_groups=-mypy \
  --features=-parse_headers --host_features=-parse_headers
COVERAGE_BAZELFLAGS = $(GENERATE_BAZELFLAGS)

compdb:
	$(BAZEL) build $(COMPDB_BAZELFLAGS) -- //...
	$(BAZEL) run $(COMPDB_BAZELFLAGS) \
	  -- @wolfd_bazel_compile_commands//:generate_compile_commands

coverage:
	$(BAZEL) run $(COVERAGE_BAZELFLAGS) \
	  -- @bazelcov --bazel='$(BAZEL)' --output=coverage-report

check-extra:
        # We don’t want any Python or Go rules in the public packages, as our
        # users would have to depend on the Python or Go rules then as well.
	! $(GIT) grep -I -r -E -n -e '@rules_(python|go)' -- elisp emacs
        # Restrict loaded Starlark files in public packages to well-known
        # official repositories to avoid dependency creep.
	! $(GIT) grep -I -r -E -n -e '^load\("@' \
	  --and --not -e '@(bazel_skylib|protobuf|rules_cc)//' \
	  -- elisp emacs
        # Find BUILD files without default visibility.  See
        # https://opensource.google/documentation/reference/thirdparty/new_license_rules#new_requirements.
	! $(GIT) grep -I -r -F -L \
	  -e 'default_visibility = ["//visibility:private"]' \
	  -- '*/BUILD' '*/BUILD.bazel'
        # Find Starlark files without visibility declaration.
	! $(GIT) grep -I -r -E -L -e '^visibility\(' -- '*.bzl'
        # Find BUILD files without correct license declaration.
	! $(GIT) grep -I -r -F -L -e 'default_applicable_licenses' \
	  -- '*/BUILD' '*/BUILD.bazel'
	! $(GIT) grep -I -r -E -L -e '^licenses\(\["notice"\]\)' \
	  -- '*/BUILD' '*/BUILD.bazel'
        # Find BUILD files without correct features.
	! $(GIT) grep -I -r -F -L -e 'features = PACKAGE_FEATURES' \
	  -- '*/BUILD' '*/BUILD.bazel' ':^/examples/ext/'
	$(GIT) grep -I -r -E -l -z -e '^#!/' -- ':^*.ps1' | \
	  xargs -t -r -0 -- $(SHELLCHECK) $(SHELLCHECKFLAGS) --

BENCHMARK_BAZELFLAGS = $(BAZELFLAGS) --lockfile_mode=off --compilation_mode=opt

benchmark:
	$(HYPERFINE) $(HYPERFINEFLAGS) --shell=none --warmup=5 \
	  --setup='$(BAZEL) build $(BENCHMARK_BAZELFLAGS) -- //tests:empty' \
	  -- \
	  '$(BAZEL) run $(BENCHMARK_BAZELFLAGS) -- //tests:empty' \
	  './bazel-bin/tests/empty' \
	  '$(EMACS) -Q -batch $(EMACSFLAGS) -l bazel-bin/tests/empty.elc'

clean:
	$(BAZEL) clean

PREFIX = /usr/local
INFODIR = $(PREFIX)/share/info
INSTALL = install
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL) -m 644
INSTALL_INFO = install-info

install:
	$(BAZEL) build $(BAZELFLAGS) -- //docs:rules_elisp.info
	$(INSTALL) -d -- '$(INFODIR)'
	$(INSTALL_DATA) -- \
	  bazel-bin/docs/rules_elisp.info \
	  '$(INFODIR)/rules_elisp.info'
	$(INSTALL_INFO) -- '$(INFODIR)/rules_elisp.info' '$(INFODIR)/dir'

uninstall:
	$(INSTALL_INFO) --delete -- \
	  '$(INFODIR)/rules_elisp.info' '$(INFODIR)/dir'
	rm -- '$(INFODIR)/rules_elisp.info'
