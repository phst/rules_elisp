// Copyright 2021-2026 Google LLC
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

// Binary stardoc2org converts Stardoc output into Org Mode format.
//
// Usage:
//
//	stardoc2org INPUT-FILE OUTPUT-FILE
//
// INPUT-FILE must contain the binary serialization of a
// stardoc_output.ModuleInfo protocol buffer message; this is usually generated
// by a starlark_doc_extract Bazel rule.  The program writes an Org-mode
// rendering of the module documentation into OUTPUT-FILE.
package main

import (
	"flag"
	"fmt"
	"log"
	"os"

	"google.golang.org/protobuf/proto"

	"github.com/phst/rules_elisp/internal/stardoc"
	spb "github.com/phst/rules_elisp/internal/stardoc_output_go_proto"
)

func main() {
	flag.Usage = usage
	flag.Parse()
	if flag.NArg() != 2 {
		usage()
		os.Exit(2)
	}
	input := flag.Arg(0)
	output := flag.Arg(1)
	if err := run(input, output); err != nil {
		log.Fatal(err)
	}
}

func usage() {
	fmt.Fprintln(os.Stderr, "Usage: stardoc2org INPUT OUTPUT")
	flag.PrintDefaults()
}

func run(in, out string) error {
	b, err := os.ReadFile(in)
	if err != nil {
		return err
	}
	var module spb.ModuleInfo
	if err := proto.Unmarshal(b, &module); err != nil {
		return err
	}
	file, err := os.OpenFile(out, os.O_CREATE|os.O_EXCL|os.O_WRONLY, 0400)
	if err != nil {
		return err
	}
	defer file.Close()
	if err := stardoc.Org(&module, file); err != nil {
		return err
	}
	if err := file.Sync(); err != nil {
		return err
	}
	return file.Close()
}
