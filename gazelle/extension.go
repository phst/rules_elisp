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

package gazelle

import (
	"log"

	"github.com/bazelbuild/bazel-gazelle/config"
)

type extension struct{}

func initExtension(c *config.Config) *extension {
	ext := getExtension(c)
	if ext != nil {
		return ext
	}
	ext = new(extension)
	c.Exts[languageName] = ext
	return ext
}

func getExtension(c *config.Config) *extension {
	switch ext := c.Exts[languageName].(type) {
	case nil:
		return nil
	case *extension:
		return ext
	default:
		log.Printf("invalid extension of type %T in configuration", ext)
		return nil
	}
}

func (e *extension) clone() *extension {
	return &extension{}
}
