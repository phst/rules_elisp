# Copyright 2021, 2023, 2024, 2025 Google LLC
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
FIND = find
GREP = grep

all: generate check

generate: compdb coverage

check: nogo
	./build.py -- check

GENERATE_BAZELFLAGS = $(BAZELFLAGS) --lockfile_mode=off
COMPDB_BAZELFLAGS = $(GENERATE_BAZELFLAGS) --output_groups=-check_python
COVERAGE_BAZELFLAGS = $(GENERATE_BAZELFLAGS)

compdb:
	$(BAZEL) run $(COMPDB_BAZELFLAGS) \
	  -- @hedron_compile_commands//:refresh_all $(COMPDB_BAZELFLAGS)

coverage:
	$(BAZEL) run $(COVERAGE_BAZELFLAGS) \
	  -- @bazelcov --bazel='$(BAZEL)' --output=coverage-report

# We don’t want any Go rules in the public packages, as our users would have to
# depend on the Go rules then as well.
nogo:
	echo 'Looking for unwanted Go targets in public packages'
	! $(FIND) elisp emacs -type f \
	  -exec $(GREP) -F -e '@rules_go' -n -- '{}' '+' \
	  || { echo 'Unwanted Go targets found'; exit 1; }

PREFIX = /usr/local
INFODIR = $(PREFIX)/share/info
INSTALL = install
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL) -m 644
INSTALL_INFO = install-info

install: check
	$(BAZEL) build $(BAZELFLAGS) -- //docs:rules_elisp.info
	$(INSTALL) -d -- '$(INFODIR)'
	$(INSTALL_DATA) -- \
	  bazel-bin/docs/rules_elisp.info \
	  '$(INFODIR)/rules_elisp.info'
	$(INSTALL_INFO) -- '$(INFODIR)/rules_elisp.info' '$(INFODIR)/dir'
