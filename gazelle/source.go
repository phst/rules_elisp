// Copyright 2021, 2022 Google LLC
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

package gazelle

import (
	"path"
	"strings"

	"github.com/bazelbuild/bazel-gazelle/label"
)

// sourceFile names an Emacs Lisp source file.  By convention, it’s a
// slash-separated clean filename relative to the main workspace root.  The
// extension should always be “.el”.
type sourceFile string

const sourceExt = ".el"

// srcFile returns the source file for the given label.  The label must be
// absolute.  srcFile returns an empty string if the label isn’t in the main
// workspace or doesn’t refer to an Emacs Lisp source file.
func srcFile(lbl label.Label) sourceFile {
	if lbl.Repo != "" {
		// Currently we don’t support sources from other repositories.
		return ""
	}
	if path.Ext(lbl.Name) != sourceExt {
		// Not an Emacs Lisp source file.
		return ""
	}
	return sourceFile(path.Join(lbl.Pkg, lbl.Name))
}

func (f sourceFile) valid() bool { return f != "" }

// stem returns the filename without extension.
func (f sourceFile) stem() string {
	return strings.TrimSuffix(string(f), sourceExt)
}

// ruleName returns a suggestion for a rule name that consumes the receiver
// file.
func (f sourceFile) ruleName() string {
	return strings.ReplaceAll(path.Base(f.stem()), "-", "_")
}

// bazelPackage names a Bazel package.  It’s typically a slash-separated
// directory name within a workspace.  The empty package name is valid and
// refers to the root package.
type bazelPackage string
