// Copyright 2020-2022, 2024, 2025 Google LLC
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

// Binary wrap is a test helper program for //elisp:binary_test, which see.
package main

import (
	"encoding/json"
	"flag"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"reflect"

	"github.com/bazelbuild/rules_go/go/runfiles"
	"github.com/google/go-cmp/cmp"
)

func main() {
	log.Println("Args:", os.Args)
	log.Println("Environment:", os.Environ())
	var manifestFile string
	flag.StringVar(&manifestFile, "manifest", "", "")
	flag.Parse()
	if manifestFile == "" {
		log.Fatal("--manifest is empty")
	}
	runfilesLib, err := runfiles.Rlocation("phst_rules_elisp/elisp/runfiles/runfiles.elc")
	if err != nil {
		log.Fatal(err)
	}
	inputFile, err := runfiles.Rlocation("phst_rules_elisp/elisp/private/tools/binary.cc")
	if err != nil {
		log.Fatal(err)
	}
	var outputFile string
	if os.PathSeparator == '/' {
		outputFile = "/tmp/output.dat"
	} else {
		outputFile = `C:\Temp\output.dat`
	}
	gotArgs := flag.Args()
	wantArgs := []string{"--quick", "--batch"}
	// The load path setup depends on whether we use manifest-based or
	// directory-based runfiles.
	if dir, err := runfiles.Rlocation("phst_rules_elisp"); err == nil {
		// Directory-based runfiles.
		wantArgs = append(wantArgs, "--directory="+dir)
	} else {
		// Manifest-based runfiles.
		wantArgs = append(wantArgs,
			"--load="+runfilesLib,
			"--funcall=elisp/runfiles/install-handler",
			"--directory=/bazel-runfile:phst_rules_elisp",
		)
	}
	wantArgs = append(wantArgs,
		"--option",
		inputFile,
		" \t\n\r\f äα𝐴🐈'\\\"",
		"/:"+outputFile,
	)
	if diff := cmp.Diff(gotArgs, wantArgs); diff != "" {
		log.Fatalf("positional arguments: -got +want:\n%s", diff)
	}
	jsonData, err := ioutil.ReadFile(manifestFile)
	if err != nil {
		log.Fatalf("can’t read manifest: %s", err)
	}
	var gotManifest map[string]any
	if err := json.Unmarshal(jsonData, &gotManifest); err != nil {
		log.Fatalf("can’t decode manifest: %s", err)
	}
	wantManifest := map[string]any{
		"root":        "RUNFILES_ROOT",
		"tags":        []any{"local", "mytag"},
		"loadPath":    []any{"phst_rules_elisp"},
		"inputFiles":  []any{"phst_rules_elisp/elisp/private/tools/binary.cc", "phst_rules_elisp/elisp/private/tools/binary.h"},
		"outputFiles": []any{outputFile},
	}
	if diff := cmp.Diff(
		gotManifest, wantManifest,
		cmp.FilterPath(isInputFile, cmp.Transformer("", resolveRunfile)),
	); diff != "" {
		log.Fatalf("manifest: -got +want:\n%s", diff)
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
