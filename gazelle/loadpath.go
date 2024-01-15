// Copyright 2021, 2022, 2024 Google LLC
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
	"regexp"
	"sort"
	"strings"

	"github.com/bazelbuild/bazel-gazelle/pathtools"
)

// loadDirectory names a directory in the Emacs load path.  See
// https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Libraries.html.
// By convention, a load directory is a nonempty clean slash-separated directory
// name (without leading or trailing slash) relative to the main repository
// root.  The main repository root itself is represented using a single dot.  An
// empty load directory is invalid.
type loadDirectory string

// loadDirFromAttr returns the load directory that corresponds to the given
// load_path attribute element, assuming the package of the surrounding rule is
// pkg.
func loadDirFromAttr(attr string, pkg bazelPackage) loadDirectory {
	if path.IsAbs(attr) {
		return loadDirectory(strings.TrimPrefix(path.Clean(attr), "/"))
	} else {
		return loadDirectory(path.Join(string(pkg), attr))
	}
}

// loadDirForFile returns the load directory that would be needed so that the
// file can provide the feature and can be found using a one-argument require
// form.  It returns the empty string if there is no such directory.
func loadDirForFile(file sourceFile, feat Feature) loadDirectory {
	stem := "/" + file.stem()
	suffix := "/" + string(feat)
	dir := strings.TrimSuffix(stem, suffix)
	if dir == stem {
		return ""
	}
	return loadDirectory(strings.TrimPrefix(dir, "/"))
}

func (d loadDirectory) valid() bool { return d != "" }

// attr returns the value that the load_path element for this directory should
// have within the given package.
func (d loadDirectory) attr(pkg bazelPackage) string {
	s := string(d)
	suffix := pathtools.TrimPrefix(s, string(pkg))
	if suffix == s {
		return "/" + s
	}
	return path.Clean(suffix)
}

// loadPath represents the Emacs load path, an ordered sequence of directories.
type loadPath []loadDirectory

// loadPathForFile returns the load path that would be needed to be able to load
// the given file with a one-argument require form given any of the provided
// features within the file.
func loadPathForFile(name sourceFile, content []byte) loadPath {
	dirs := make(map[loadDirectory]struct{})
	for _, m := range providePattern.FindAllSubmatch(content, -1) {
		feat := Feature(m[1])
		// Ignore features that couldn’t be used with a simple ‘require’
		// form.  Only if the file name is of the form “FEATURE” or
		// “DIR/FEATURE”, we can ‘require’ the feature from this file.
		// In the latter case, we need DIR in the load path.
		dir := loadDirForFile(name, feat)
		if !dir.valid() {
			continue
		}
		dirs[dir] = struct{}{}
	}
	var r loadPath
	for dir := range dirs {
		r = append(r, dir)
	}
	sort.Sort(r)
	return r
}

var providePattern = regexp.MustCompile(`(?m)^\(provide '([-/\w]+)\)`)

// loadDirFromAttr returns the load path that corresponds to the given load_path
// attribute value, assuming the package of the surrounding rule is pkg.
func loadPathFromAttr(attr []string, pkg bazelPackage) loadPath {
	r := loadPath{"."} // implicitly added
	for _, s := range attr {
		r = append(r, loadDirFromAttr(s, pkg))
	}
	return r
}

func (p loadPath) Len() int           { return len(p) }
func (p loadPath) Less(i, j int) bool { return p[i] < p[j] }
func (p loadPath) Swap(i, j int)      { p[i], p[j] = p[j], p[i] }

// attr returns the corresponding value for load_path attributes within the
// given package.
func (p loadPath) attr(pkg bazelPackage) []string {
	var r []string
	for _, d := range p {
		s := d.attr(pkg)
		if s == "/" { // implicitly added
			continue
		}
		r = append(r, s)
	}
	return r
}
