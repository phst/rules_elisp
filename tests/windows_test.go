// Copyright 2020-2023, 2025 Google LLC
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

//go:build windows

package tests_test

const outputFile = `C:\Temp\output.dat`

// Emacs exits with a code of −1 in case of a signal, which gets converted to an
// exit status with all bits set: 8 bits on Unix systems, 32 bits on Windows.
const signalCode = 0xFFFFFFFF
