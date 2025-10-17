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

package main

import (
	"flag"
	"os"
	"time"
)

func main() {
	var exit int
	var sleep time.Duration
	flag.IntVar(&exit, "exit", 0, "exit code")
	flag.DurationVar(&sleep, "sleep", 0, "how long to sleep")
	flag.Parse()
	time.Sleep(sleep)
	os.Exit(exit)
}
