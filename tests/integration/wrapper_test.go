// Copyright 2020-2023, 2025, 2026 Google LLC
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

package integration_test

import (
	"encoding/json"
	"log"
	"os/exec"
	"path/filepath"
	"reflect"
	"slices"
	"testing"

	"github.com/bazelbuild/rules_go/go/runfiles"
	"github.com/google/go-cmp/cmp"

	"github.com/phst/rules_elisp/internal/testutil"
)

var (
	launcher    = testutil.RunfileFlag("//internal/wrap:launcher")
	binaryH     = testutil.RunfileFlag("//elisp/private/tools:binary.h")
	binaryCc    = testutil.RunfileFlag("//elisp/private/tools:binary.cc")
	runfilesElc = testutil.RunfileFlag("//elisp/runfiles:runfiles.elc")
)

// Test that running a binary with a wrapper works.
func TestWrapper(t *testing.T) {
	cmd := exec.Command(
		*launcher,
		"--option",
		*binaryCc,
		" \t\n\r\f äα𝐴🐈'\\\"",
		"/:"+outputFile,
	)
	cmd.Stderr = t.Output()
	out, err := cmd.Output()
	if err != nil {
		t.Fatal(err)
	}
	var got struct {
		Args     []string
		Manifest string
	}
	if err := json.Unmarshal(out, &got); err != nil {
		t.Fatal(err)
	}
	runfilesLib := *runfilesElc
	gotArgs := got.Args
	dir, err := runfiles.Rlocation("rules_elisp")
	if err != nil {
		dir = "ERROR \u0000" // this can never match
	}
	// The load path setup depends on whether we use manifest-based or
	// directory-based runfiles.
	wantArgs := [][]string{
		// Directory-based runfiles.
		{
			"--quick",
			"--batch",
			"--directory=" + dir,
			"--option",
			*binaryCc,
			" \t\n\r\f äα𝐴🐈'\\\"",
			"/:" + outputFile,
		},
		// Manifest-based runfiles.
		{
			"--quick",
			"--batch",
			"--load=" + runfilesLib,
			"--funcall=elisp/runfiles/install-handler",
			"--directory=/bazel-runfile:rules_elisp",
			"--option",
			*binaryCc,
			" \t\n\r\f äα𝐴🐈'\\\"",
			"/:" + outputFile,
		},
	}
	var diffs []string
	for _, want := range wantArgs {
		diffs = append(diffs, cmp.Diff(gotArgs, want))
	}
	// Pick the smaller difference for reporting.
	if diff := slices.MinFunc(diffs, func(a, b string) int { return len(a) - len(b) }); diff != "" {
		t.Errorf("positional arguments: -got +want:\n%s", diff)
	}
	jsonData := []byte(got.Manifest)
	var gotManifest map[string]any
	if err := json.Unmarshal(jsonData, &gotManifest); err != nil {
		t.Fatalf("can’t decode manifest: %s", err)
	}
	wantManifest := map[string]any{
		"root":        "RUNFILES_ROOT",
		"tags":        []any{"local", "mytag"},
		"loadPath":    []any{"rules_elisp"},
		"inputFiles":  []any{*binaryCc, *binaryH},
		"outputFiles": []any{outputFile},
	}
	if diff := cmp.Diff(
		gotManifest, wantManifest,
		cmp.FilterPath(isInputFile, cmp.Transformer("", resolveRunfile)),
	); diff != "" {
		t.Errorf("manifest: -got +want:\n%s", diff)
	}
}

func isInputFile(p cmp.Path) bool {
	if len(p) < 2 {
		return false
	}
	m, ok := p[1].(cmp.MapIndex)
	if !ok {
		return false
	}
	k := m.Key()
	return k.Kind() == reflect.String && k.String() == "inputFiles"
}

func resolveRunfile(s string) string {
	if filepath.IsAbs(s) {
		return s
	}
	r, err := runfiles.Rlocation(s)
	if err != nil {
		log.Fatalf("error resolving runfile for comparison: %s", err)
	}
	return r
}
