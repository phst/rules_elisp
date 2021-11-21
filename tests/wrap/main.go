// Copyright 2020, 2021 Google LLC
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
	"reflect"

	"github.com/google/go-cmp/cmp"
	"github.com/phst/runfiles"
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
	runfilesLib, err := runfiles.Path("phst_rules_elisp/elisp/runfiles/runfiles.elc")
	if err != nil {
		log.Fatal(err)
	}
	// The load path setup depends on whether we use manifest-based or
	// directory-based runfiles.
	var loadPathArgs []string
	if dir, err := runfiles.Path("phst_rules_elisp"); err == nil {
		// Directory-based runfiles.
		loadPathArgs = []string{"--directory=" + dir}
	} else {
		// Manifest-based runfiles.
		loadPathArgs = []string{
			"--load=" + runfilesLib,
			"--funcall=elisp/runfiles/install-handler",
			"--directory=/bazel-runfile:phst_rules_elisp",
		}
	}
	inputFile, err := runfiles.Path("phst_rules_elisp/elisp/binary.cc")
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
	wantArgs := append(
		append([]string{"--quick", "--batch"}, loadPathArgs...),
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
	var gotManifest map[string]interface{}
	if err := json.Unmarshal(jsonData, &gotManifest); err != nil {
		log.Fatalf("can’t decode manifest: %s", err)
	}
	wantManifest := map[string]interface{}{
		"root":        "RUNFILES_ROOT",
		"tags":        []interface{}{"local", "mytag"},
		"loadPath":    []interface{}{"phst_rules_elisp"},
		"inputFiles":  []interface{}{"phst_rules_elisp/elisp/binary.cc", "phst_rules_elisp/elisp/binary.h"},
		"outputFiles": []interface{}{outputFile},
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
	r, err := runfiles.Path(s)
	if err != nil {
		log.Fatal(err)
	}
	return r
}
