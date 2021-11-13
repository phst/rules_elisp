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

	"github.com/google/go-cmp/cmp"
	"github.com/phst/runfiles"
)

func main() {
	log.Println("Args:", os.Args)
	var manifestFile string
	flag.StringVar(&manifestFile, "manifest", "", "")
	flag.Parse()
	if manifestFile == "" {
		log.Fatal("--manifest is empty")
	}
	workspaceDir, err := runfiles.Path("phst_rules_elisp")
	if err != nil {
		log.Fatal(err)
	}
	gotArgs := flag.Args()
	wantArgs := []string{
		"--quick", "--batch",
		"--directory=" + workspaceDir,
		"--option",
		"elisp/binary.cc",
		"/:/tmp/output.dat",
	}
	if diff := cmp.Diff(gotArgs, wantArgs); diff != "" {
		log.Fatalf("positional arguments: -git +want:\n%s", diff)
	}
	jsonData, err := ioutil.ReadFile(manifestFile)
	if err != nil {
		log.Fatalf("can’t read manifest: %s", err)
	}
	var got map[string]interface{}
	if err := json.Unmarshal(jsonData, &got); err != nil {
		log.Fatalf("can’t decode manifest: %s", err)
	}
	want := map[string]interface{}{
		"root":        "RUNFILES_ROOT",
		"tags":        []interface{}{"local", "mytag"},
		"loadPath":    []interface{}{"phst_rules_elisp"},
		"inputFiles":  []interface{}{"phst_rules_elisp/elisp/binary.cc", "phst_rules_elisp/elisp/binary.h"},
		"outputFiles": []interface{}{"/tmp/output.dat"},
	}
	if diff := cmp.Diff(got, want); diff != "" {
		log.Fatalf("manifest: -got +want:\n%s", diff)
	}
}
