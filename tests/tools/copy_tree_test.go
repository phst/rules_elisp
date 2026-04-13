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

package tools_test

import (
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
	"testing/fstest"
	"time"

	"github.com/google/go-cmp/cmp"
	"github.com/phst/rules_elisp/private/testutil"
)

var copyTree = testutil.RunfileFlag("//elisp/private/tools:copy_tree")

func TestCopyTree(t *testing.T) {
	srcDir := t.TempDir()
	destDir := t.TempDir()
	sentinel := filepath.Join(srcDir, "files.txt")

	fd, err := os.Create(sentinel)
	if err != nil {
		t.Fatal(err)
	}
	defer fd.Close()
	mtime := time.Date(2022, time.July, 8, 0, 0, 0, 0, time.UTC)
	for _, n := range []string{"foo", "bar"} {
		p := filepath.Join(srcDir, n)
		if err := os.WriteFile(p, nil, 0400); err != nil {
			t.Fatal(err)
		}
		if err := os.Chtimes(p, time.Time{}, mtime); err != nil {
			t.Fatal(err)
		}
		if _, err := fmt.Fprintln(fd, p); err != nil {
			t.Fatal(err)
		}
	}
	if err := fd.Close(); err != nil {
		t.Fatal(err)
	}

	cmd := exec.Command(*copyTree, sentinel, destDir, sentinel)
	out, err := cmd.CombinedOutput()
	t.Logf("copy_tree output:\n%s", out)
	if err != nil {
		t.Fatal(err)
	}
	if n := len(out); n != 0 {
		t.Errorf("copy_tree produced %d bytes of output", n)
	}

	got := allFiles(t, os.DirFS(destDir))
	wantFiles := fstest.MapFS{
		"foo": {Mode: 0400, ModTime: mtime},
		"bar": {Mode: 0400, ModTime: mtime},
	}
	want := allFiles(t, wantFiles)
	if diff := cmp.Diff(got, want); diff != "" {
		t.Errorf("-got +want:\n%s", diff)
	}
}

type fileInfo struct {
	Name string
	Type fs.FileMode
	Time time.Time
}

func allFiles(t *testing.T, fsys fs.FS) []fileInfo {
	var infos []fileInfo
	fn := func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		i, err := d.Info()
		if err != nil {
			return err
		}
		if path == "." {
			return nil
		}
		infos = append(infos, fileInfo{path, d.Type(), i.ModTime()})
		return nil
	}
	if err := fs.WalkDir(fsys, ".", fn); err != nil {
		t.Error(err)
		return nil
	}
	return infos
}
