// Copyright 2021, 2022, 2023, 2025 Google LLC
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

package gazelle_x_test

import (
	"flag"
	"path/filepath"
	"testing"
	"time"

	"github.com/bazelbuild/bazel-gazelle/testtools"
	"github.com/bazelbuild/rules_go/go/runfiles"
)

var (
	gazelleBinary    = flag.String("gazelle", "", "location of the Gazelle binary")
	testdataSentinel = flag.String("testdata", "", "runfile path of the //gazelle/testdata:BUILD file")
)

func TestGazelleBinary(t *testing.T) {
	rf, err := runfiles.New()
	if err != nil {
		t.Fatal(err)
	}
	binary, err := rf.Rlocation(*gazelleBinary)
	if err != nil {
		t.Fatal(err)
	}
	sentinel, err := rf.Rlocation(*testdataSentinel)
	if err != nil {
		t.Fatal(err)
	}
	args := &testtools.TestGazelleGenerationArgs{
		Name:                 "update",
		TestDataPathAbsolute: filepath.Join(filepath.Dir(sentinel), "update"),
		TestDataPathRelative: "gazelle/testdata/update",
		GazelleBinaryPath:    binary,
		Timeout:              time.Minute,
	}
	testtools.TestGazelleGenerationOnPath(t, args)
}
