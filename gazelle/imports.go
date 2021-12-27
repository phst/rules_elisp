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
	"log"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/label"
	"github.com/bazelbuild/bazel-gazelle/pathtools"
	"github.com/bazelbuild/bazel-gazelle/resolve"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

// Imports implements Resolver.Imports.  For an elisp_library rule, it returns
// the features that the source files likely provide.  It doesn’t actually parse
// the source files.
func (elisp) Imports(c *config.Config, r *rule.Rule, f *rule.File) []resolve.ImportSpec {
	if r.Kind() != libraryKind {
		return nil
	}
	pkg := bazelPackage(f.Pkg)
	srcs := r.AttrStrings("srcs")
	load := loadPathFromAttr(r.AttrStrings("load_path"), pkg)
	imports := []resolve.ImportSpec{} // never nil
	for _, src := range srcs {
		lbl, err := label.Parse(src)
		if err != nil {
			log.Printf("invalid source filename %q: %s", src, err)
			continue
		}
		src := srcFile(lbl.Abs("", string(pkg)))
		if !src.valid() {
			continue
		}
		feat := potentialProvides(src, load)
		imports = append(imports, feat...)
	}
	return imports
}

// potentialProvides returns the features that the file might provide, assuming
// the load path is loadPath.
func potentialProvides(file sourceFile, loadPath loadPath) []resolve.ImportSpec {
	var r []resolve.ImportSpec
	for _, dir := range loadPath {
		feat := potentialProvide(file, dir)
		if !feat.valid() {
			// The source file is not within the load path element.
			continue
		}
		r = append(r, feat.importSpec())
	}
	return r
}

// potentialProvide returns the name of the feature that the file might provide,
// assuming dir is a directory in the load path.  It returns the empty string if
// there is no such feature.
func potentialProvide(file sourceFile, dir loadDirectory) Feature {
	stem := file.stem()
	if dir == "." {
		return Feature(stem)
	}
	s := pathtools.TrimPrefix(stem, string(dir))
	if s == stem {
		return ""
	}
	return Feature(s)
}
