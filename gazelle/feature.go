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

import "github.com/bazelbuild/bazel-gazelle/resolve"

// Feature names an Emacs Lisp feature.  See
// https://www.gnu.org/software/emacs/manual/html_node/elisp/Named-Features.html.
// The empty Feature is invalid.
type Feature string

func (f Feature) valid() bool { return f != "" }

func (f Feature) importSpec() resolve.ImportSpec {
	return resolve.ImportSpec{
		Lang: languageName,
		Imp:  string(f),
	}
}

type features []Feature

func (f features) Len() int           { return len(f) }
func (f features) Less(i, j int) bool { return f[i] < f[j] }
func (f features) Swap(i, j int)      { f[i], f[j] = f[j], f[i] }
