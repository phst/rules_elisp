// Copyright 2023-2026 Google LLC
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

module github.com/phst/rules_elisp

go 1.26.7

require (
	github.com/bazelbuild/bazel-gazelle v0.54.0
	github.com/bazelbuild/buildtools v0.0.0-20260903221031-d00ce382946a
	github.com/bazelbuild/rules_go v0.63.0
	github.com/google/go-cmp v0.7.0
	github.com/yuin/goldmark/v2 v2.0.1
	golang.org/x/text v0.41.0
	google.golang.org/protobuf v1.36.12
)

require (
	github.com/bazel-contrib/bazel-gazelle/v2 v2.0.0-3 // indirect
	github.com/bmatcuk/doublestar/v4 v4.9.1 // indirect
	github.com/google/addlicense v1.2.0 // indirect
	github.com/inconshreveable/mousetrap v1.1.0 // indirect
	github.com/lvthillo/gomodzip v0.2.0
	github.com/spf13/cobra v1.10.2 // indirect
	github.com/spf13/pflag v1.0.9 // indirect
	golang.org/x/mod v0.38.0 // indirect
	golang.org/x/sync v0.22.0 // indirect
	golang.org/x/sys v0.41.0 // indirect
	golang.org/x/tools/go/vcs v0.1.0-deprecated // indirect
)

tool (
	github.com/google/addlicense
	github.com/lvthillo/gomodzip
)
