// Copyright 2020 Google LLC
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

// Binary wrap is a test helper program for //emacs:wrap_test, which see.
package main

import (
	"encoding/json"
	"flag"
	"io/ioutil"
	"log"
	"os"

	"github.com/google/go-cmp/cmp"
)

func main() {
	log.Println("Args:", os.Args)
	var manifestFile string
	flag.StringVar(&manifestFile, "manifest", "", "")
	flag.Parse()
	if manifestFile == "" {
		log.Fatal("--manifest is empty")
	}
	if flag.NArg() == 0 {
		log.Fatal("no positional arguments")
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
		"root":       "RUNFILES_ROOT",
		"loadPath":   []interface{}{"phst_rules_elisp"},
		"inputFiles": []interface{}{"phst_rules_elisp/emacs/exec.h"},
	}
	if diff := cmp.Diff(got, want); diff != "" {
		log.Fatalf("manifest: -got +want:\n%s", diff)
	}
}
