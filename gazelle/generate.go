// Copyright 2022, 2023, 2025 Google LLC
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
	"io/fs"
	"log"
	"os"
	"regexp"
	"slices"
	"sort"
	"strings"

	"github.com/bazelbuild/bazel-gazelle/label"
	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

// GenerateRules implements [language.Language.GenerateRules].  It generates
// elisp_library, elisp_proto_library, or elisp_test rules, one per source file.
// It adds the features required by each file to the
// [language.GenerateResult.Imports] slice as [imports] slices.
func (elisp) GenerateRules(args language.GenerateArgs) language.GenerateResult {
	fsys := os.DirFS(args.Dir)
	pkg := bazelPackage(args.Rel)
	var res language.GenerateResult
	for _, file := range args.RegularFiles {
		// We generate exactly one rule per file.  This should work fine
		// unless there are dependency cycles which cannot be resolved
		// at compile time.
		r, i := generateRule(fsys, pkg, file)
		if r == nil {
			continue
		}
		res.Gen = append(res.Gen, r)
		res.Imports = append(res.Imports, i)
	}
	sort.Sort(byName(res)) // make output deterministic
	return res
}

// generateRule generates a rule for a single Emacs Lisp or protocol buffer
// definition source file.  fsys is a filesystem for the Bazel workspace root
// containing the source file, pkg is the name of the Bazel package (the empty
// string for the root package), and file names the source file within the
// package.  Return nil if file doesn’t name an Emacs Lisp or protocol buffer
// definition file or on any error.
func generateRule(fsys fs.FS, pkg bazelPackage, file string) (*rule.Rule, imports) {
	if file == ".dir-locals.el" {
		// Never generate a rule for .dir-locals.el, as it can’t be
		// compiled.
		return nil, nil
	}
	if stem, ok := strings.CutSuffix(file, ".proto"); ok && stem != "" {
		r := rule.NewRule(protoLibraryKind, stem+"_elisp_proto")
		r.SetAttr("deps", []string{":" + stem + "_proto"})
		return r, nil
	}
	if stem, ok := strings.CutSuffix(file, ".org"); ok && stem != "" {
		r := rule.NewRule(manualKind, stem)
		r.SetAttr("src", file)
		r.SetAttr("out", stem+".texi")
		return r, nil
	}
	src := srcFile(label.New("", string(pkg), file))
	if !src.valid() {
		// Probably not an Emacs Lisp file.  Don’t print an error.
		return nil, nil
	}
	b, err := fs.ReadFile(fsys, file)
	if err != nil {
		log.Printf("unreadable source file %s: %s", file, err)
		return nil, nil
	}
	// We assume that most files are libraries and only assume a test file
	// if the filename makes that likely.  We don’t look at the file
	// contents at all: libraries can legitimately contain ‘ert-deftest’
	// forms, and we want to support empty files, too.  We never detect
	// binaries: they are rather rare and hard to distinguish from
	// libraries.
	kind := libraryKind
	if testFilePattern.MatchString(file) {
		kind = testKind
	}
	var loadPath loadPath
	if kind == libraryKind { // only libraries have a load path
		loadPath = loadPathForFile(src, b)
	}
	r := rule.NewRule(kind, src.ruleName())
	r.SetAttr("srcs", []string{file})
	if len(loadPath) > 0 {
		r.SetAttr("load_path", loadPath.attr(pkg))
	}
	// Also return required features.
	requiresMap := make(map[feature]struct{})
	for _, m := range requirePattern.FindAllSubmatch(b, -1) {
		feat := feature(m[1])
		if _, ok := builtinFeatures[feat]; ok {
			continue
		}
		requiresMap[feat] = struct{}{}
	}
	var requires []feature
	for f := range requiresMap {
		requires = append(requires, f)
	}
	slices.Sort(requires)
	return r, requires
}

// imports documents which features an Emacs Lisp file requires.  The
// implementation of [language.Language.GenerateRules] returned by [NewLanguage]
// uses this type for [language.GenerateResult.Imports].
type imports []feature

var (
	testFilePattern = regexp.MustCompile(`(?:^|[-_])(?:unit)?tests?\.el$`)
	requirePattern  = regexp.MustCompile(`(?m)^\(require '([-./\w]+)\)`)
)

type byName language.GenerateResult

func (s byName) Len() int {
	return len(s.Gen)
}

func (s byName) Less(i, j int) bool {
	return s.Gen[i].Name() < s.Gen[j].Name()
}

func (s byName) Swap(i, j int) {
	s.Gen[i], s.Gen[j] = s.Gen[j], s.Gen[i]
	s.Imports[i], s.Imports[j] = s.Imports[j], s.Imports[i]
}
