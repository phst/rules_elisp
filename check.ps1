#!/usr/bin/env pwsh

# Copyright 2021, 2022, 2023, 2024, 2025 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#Requires -Version 7.4

param ([switch]$Coverage)

$ErrorActionPreference = 'Stop'
$PSNativeCommandUseErrorActionPreference = $true

$candidates = @(
    Get-Command -Name bazelisk, bazel -Type Application, ExternalScript
)

if (! $candidates) {
    throw 'neither Bazelisk nor Bazel found'
}

$bazel = $candidates[0].Path

function Run-Bazel {
    Write-Verbose -Message "cd $(Get-Location) && ${bazel} ${args}"
    & $bazel @args
}

function Run-Tests {
    Run-Bazel 'test' @args '--' '//...'
    if ($Coverage) {
        Run-Bazel 'coverage' @args '--' '//...'
    }
}

# All potentially supported Emacs versions.
$versions = '29.4', '30.2'

$VerbosePreference = 'Continue'

Set-Location -Path $PSScriptRoot

# Test both default toolchain and versioned toolchains.
Run-Tests

foreach ($version in $versions) {
    Run-Tests "--extra_toolchains=//elisp:emacs_${version}_toolchain"
}

Run-Bazel 'mod' 'graph' > $null

Join-Path -Path examples -ChildPath ext | Set-Location
Run-Tests
Run-Bazel 'mod' 'graph' > $null
