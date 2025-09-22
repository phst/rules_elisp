// Copyright 2022, 2025 Google LLC
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
	"encoding/json"
	"fmt"

	_ "embed"
)

var builtinFeatures = decodeBuiltinFeatures(builtinFeaturesJSON)

func decodeBuiltinFeatures(b []byte) map[Feature]struct{} {
	var data struct {
		BuiltinFeatures []string `json:"builtinFeatures"`
	}
	if err := json.Unmarshal(b, &data); err != nil {
		panic(fmt.Errorf("invalid builtin features JSON: %s", err))
	}
	if len(data.BuiltinFeatures) == 0 {
		panic("no builtin features")
	}
	m := make(map[Feature]struct{}, len(data.BuiltinFeatures))
	for _, f := range data.BuiltinFeatures {
		m[Feature(f)] = struct{}{}
	}
	return m
}

//go:embed builtin_features.json
var builtinFeaturesJSON []byte
