# Copyright 2021, 2023, 2024 Google LLC
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

all: generate check

generate: compdb coverage

check:
	./build.py -- check

GENERATE_BAZELFLAGS = $(BAZELFLAGS) --enable_bzlmod --lockfile_mode=off
COMPDB_BAZELFLAGS = $(GENERATE_BAZELFLAGS) --output_groups=-check_python
COVERAGE_BAZELFLAGS = $(GENERATE_BAZELFLAGS)

compdb:
	$(BAZEL) run $(COMPDB_BAZELFLAGS) \
	  -- @hedron_compile_commands//:refresh_all $(COMPDB_BAZELFLAGS)

coverage:
	$(BAZEL) run $(COVERAGE_BAZELFLAGS) \
	  -- @phst_bazelcov//:bazelcov \
	  --bazel='$(BAZEL)' --output=coverage-report

INFODIR = /usr/local/share/info

install:
	$(BAZEL) build $(BAZELFLAGS) -- //docs:rules_elisp.info
	install -d -- '$(INFODIR)'
	install -m 0644 -- \
	  bazel-bin/docs/rules_elisp.info \
	  '$(INFODIR)/rules_elisp.info'
	install-info -- '$(INFODIR)/rules_elisp.info' '$(INFODIR)/dir'
