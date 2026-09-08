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

// Helper binary to convert Stardoc output into Org Mode format.
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
	b, err := os.ReadFile(input)
	if err != nil {
		log.Fatal(err)
	}
	var module spb.ModuleInfo
	if err := proto.Unmarshal(b, &module); err != nil {
		log.Fatal(err)
	}
	file, err := os.OpenFile(output, os.O_CREATE|os.O_EXCL|os.O_WRONLY, 0400)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()
	if err := stardoc.Org(&module, file); err != nil {
		log.Fatal(err)
	}
	if err := file.Sync(); err != nil {
		log.Fatal(err)
	}
	if err := file.Close(); err != nil {
		log.Fatal(err)
	}
}

func usage() {
	fmt.Fprintln(os.Stderr, "Usage: stardoc2org INPUT OUTPUT")
	flag.PrintDefaults()
}
