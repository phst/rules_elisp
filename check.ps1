#!/usr/bin/env pwsh

# Copyright 2021-2026 Google LLC
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

param ([switch]$Coverage, [string[]]$BazelVersions)

Set-PSDebug -Strict
Set-StrictMode -Version 'latest'

$ErrorActionPreference = 'Stop'
$PSNativeCommandUseErrorActionPreference = $true

$candidates = @(
    Get-Command -Name bazelisk -Type Application, ExternalScript
)

if (! $candidates) {
    throw 'Bazelisk not found'
}

$bazel = $candidates[0].Path

[bool]$github = [bool]$Env:CI

function Begin-Group {
    param ([string]$message)
    [string]$prefix = $github ? '::group::' : '>>> '
    Write-Host "${prefix}${message}" -ForegroundColor 'DarkCyan'
}

function End-Group {
    if ($github) {
        Write-Host '::endgroup::' -ForegroundColor 'DarkCyan'
    }
}

function Run-Bazel {
    param ([string]$Version)
    $prefix = $Version ? "USE_BAZEL_VERSION=${Version} " : ''
    Begin-Group "cd $(Get-Location) && ${prefix}${bazel} ${args}"
    if ($Version) {
        New-Item -Verbose -Path Env: -Name USE_BAZEL_VERSION -Value $Version
    }
    & $bazel @args
    if ($Version) {
        Remove-Item -Verbose -Path Env:USE_BAZEL_VERSION
    }
    End-Group
}

function Run-Tests {
    param ([string]$Version)
    Run-Bazel -Version $Version -- 'test' @args '--' '//...'
    if ($Coverage) {
        Run-Bazel -Version $Version -- 'coverage' @args '--' '//...'
    }
}

# All supported Emacs major versions.
$versions = '30', '31'

$VerbosePreference = 'Continue'

Set-Location -Path $PSScriptRoot

if (Test-Path Env:USE_BAZEL_VERSION) {
    Write-Warning -Message 'Removing environment variable USE_BAZEL_VERSION'
    Remove-Item -Verbose -Path Env:USE_BAZEL_VERSION
}

# Test both default toolchain and versioned toolchains.
Run-Tests -Version $null

foreach ($version in $versions) {
    $toolchains = @(
        "//elisp:emacs_${version}_toolchain",
        "//elisp:emacs_${version}_windows_x86_64_toolchain"
    )
    Run-Tests -Version $null -- "--extra_toolchains=$($toolchains -join ',')"
}

Run-Tests -Version $null -- '--extra_toolchains=//elisp:local_toolchain'

Run-Bazel -Version $null -- 'mod' 'graph' > $null

# Run the Bazel tests for all supported Bazel versions.
foreach ($version in $BazelVersions) {
    # The lockfile format differs between the Bazel versions, so only for one
    # version --lockfile_mode=error can work.  --lockfile_mode=update would be
    # useless in GitHub since we never use the updated lockfiles, so switch
    # lockfiles off entirely in other Bazel versions.
    Run-Tests -Version $version -- '--lockfile_mode=off'
}

Join-Path -Path examples -ChildPath ext | Set-Location
Run-Tests -Version $null
Run-Bazel -Version $null -- 'mod' 'graph' > $null

foreach ($version in $BazelVersions) {
    # The lockfile format differs between the Bazel versions, so only for one
    # version --lockfile_mode=error can work.  --lockfile_mode=update would be
    # useless in GitHub since we never use the updated lockfiles, so switch
    # lockfiles off entirely in other Bazel versions.
    Run-Tests -Version $version -- '--lockfile_mode=off'
}
