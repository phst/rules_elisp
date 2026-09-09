// Copyright 2026 Philipp Stephani
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

package stardoc_test

import (
	"archive/tar"
	"bytes"
	"io"
	"os"
	"testing"

	_ "embed"

	"google.golang.org/protobuf/proto"

	"github.com/phst/rules_elisp/internal/stardoc"
	spb "github.com/phst/rules_elisp/internal/stardoc_output_go_proto"
)

func ExampleOrg() {
	mod := &spb.ModuleInfo{RuleInfo: []*spb.RuleInfo{{
		RuleName:  "myrule",
		DocString: "An example rule",
		OriginKey: &spb.OriginKey{Name: "myrule", File: "myrule.bzl"},
	}}}
	stardoc.Org(mod, os.Stdout)
	// Output:
	// #+ATTR_TEXINFO: :options Rule myrule ()
	// #+BEGIN_deffn
	//
	// #+BEGIN_SRC bazel-starlark
	// load("myrule.bzl", "myrule")
	// #+END_SRC
	//
	// An example rule
	//
	// #+END_deffn
}

func TestOrg(t *testing.T) {
	arch := tar.NewReader(bytes.NewReader(archiveBytes))
	for {
		h, err := arch.Next()
		if err == io.EOF {
			break
		}
		if err != nil {
			t.Fatal(err)
		}
		if h.Typeflag != tar.TypeReg {
			continue
		}
		t.Run(h.Name, func(t *testing.T) {
			b, err := io.ReadAll(arch)
			if err != nil {
				t.Fatal(err)
			}
			var info spb.ModuleInfo
			if err := proto.Unmarshal(b, &info); err != nil {
				t.Fatal(err)
			}
			var w bytes.Buffer
			if err := stardoc.Org(&info, &w); err != nil {
				t.Fatal(err)
			}
			if w.Len() == 0 {
				t.Error("Org produced empty output")
			}
		})
	}
}

//go:embed protos.tar
var archiveBytes []byte
