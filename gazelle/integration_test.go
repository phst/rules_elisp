// Copyright 2021-2023, 2025 Google LLC
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

package gazelle_test

import (
	"path/filepath"
	"testing"
	"time"

	"github.com/bazelbuild/bazel-gazelle/testtools"
	"github.com/phst/rules_elisp/private/testutil"
)

var (
	gazelleBinary    = testutil.RunfileFlag("//gazelle:wrapper")
	testdataSentinel = testutil.RunfileFlag("//gazelle/testdata:BUILD")
)

func TestGazelleBinary(t *testing.T) {
	args := &testtools.TestGazelleGenerationArgs{
		Name:                 "update",
		TestDataPathAbsolute: filepath.Join(filepath.Dir(*testdataSentinel), "update"),
		TestDataPathRelative: "gazelle/testdata/update",
		GazelleBinaryPath:    *gazelleBinary,
		Timeout:              time.Minute,
	}
	// Set GOCOVERDIR to avoid a warning message if coverage is enabled.
	// FIXME: Remove this once
	// https://github.com/bazel-contrib/rules_go/issues/3513 is fixed.
	t.Setenv("GOCOVERDIR", t.TempDir())
	testtools.TestGazelleGenerationOnPath(t, args)
}
