// Copyright 2025 Philipp Stephani
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

// Package testutil contains internal test-related utilities.
package testutil

import (
	"flag"
	"fmt"

	"github.com/bazelbuild/rules_go/go/runfiles"
)

// RunfileFlag defines a command-line flag that receives the runfile location as
// returned by the Bazel $(rlocationpath …) construct and resolves it to a local
// filename using [runfiles.Rlocation].  The flag name is usually the absolute
// Bazel label of the runfile target.  The resolved filename is stored in the
// string that the return value points to.
func RunfileFlag(name string) *string {
	r := new(runfileFlagValue)
	flag.Var(r, name, fmt.Sprintf("location of %s relative to the runfiles root", name))
	return (*string)(r)
}

type runfileFlagValue string

func (v runfileFlagValue) String() string {
	return string(v)
}

func (v *runfileFlagValue) Set(s string) error {
	p, err := runfiles.Rlocation(s)
	if err != nil {
		return err
	}
	*v = runfileFlagValue(p)
	return nil
}
