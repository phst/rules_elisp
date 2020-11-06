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

package consistency_test

import (
	"flag"
	"io/ioutil"
	"path/filepath"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"
	"golang.org/x/text/encoding"
	"golang.org/x/text/encoding/charmap"
	"golang.org/x/text/transform"
)

func Test(t *testing.T) {
	if flag.NArg() == 0 {
		t.Error("no Markdown files given on the command line")
	}
	checkedIn := make(map[string]string)
	generated := make(map[string]string)
	const (
		checkedInSuffix = ".md"
		generatedSuffix = "_doc.md"
	)
	for _, arg := range flag.Args() {
		if filepath.Ext(arg) != ".md" {
			t.Errorf("file %s is not a Markdown file", arg)
		}
		if strings.HasSuffix(arg, generatedSuffix) {
			read(t, arg, generatedSuffix, generated)
		} else {
			read(t, arg, checkedInSuffix, checkedIn)
		}
	}
	// Bazel (including Stardoc) interprets all files as Latin-1,
	// cf. https://docs.bazel.build/versions/3.0.0/build-ref.html#BUILD_files.
	// However, our files all use UTF-8, leading to double encoding.
	// Reverse that effect here.
	enc := encoding.Encoder{
		Transformer: transform.Chain(encoding.UTF8Validator, charmap.ISO8859_1.NewEncoder()),
	}
	for k, v := range generated {
		s, err := enc.String(v)
		if err != nil {
			t.Errorf("unexpected encoding in file %s: %s", k, err)
		}
		generated[k] = s
	}
	if diff := cmp.Diff(checkedIn, generated); diff != "" {
		t.Error(`Generated and check-in documentations differ.  Please run docs.py.
Diff (-checked-in +generated):
`, diff)
	}
}

func read(t *testing.T, filename, suffix string, r map[string]string) {
	t.Helper()
	if !strings.HasSuffix(filename, suffix) {
		t.Errorf("filename %s doesn’t end with expected suffix %s", filename, suffix)
		return
	}
	stem := strings.TrimSuffix(filename, suffix)
	if _, dup := r[stem]; dup {
		t.Errorf("duplicate file %s", filename)
		return
	}
	b, err := ioutil.ReadFile(filename)
	if err != nil {
		t.Error(err)
		return
	}
	r[stem] = string(b)
}
