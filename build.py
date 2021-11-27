#!/usr/bin/env python3

# Copyright 2021 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Builds this project.

Mimics a trivial version of Make."""

import argparse
import functools
import os
import pathlib
import platform
import shlex
import shutil
import subprocess
import sys
from typing import (Callable, Dict, FrozenSet, Iterable, Mapping, Optional,
                    Sequence)

def build(goals: Sequence[str]) -> None:
    """Builds the specified goals."""
    if not goals:
        raise ValueError('no goals provided')
    funcs = []
    for goal in goals:
        func = _targets.get(goal)
        if not func:
            raise KeyError(f'unknown goal {goal}')
        funcs.append(func)
    for func in funcs:
        func()

_Target = Callable[[], None]
_targets = {}  # type: Dict[str, _Target]

def target(func: _Target) -> _Target:
    """Decorator to mark a function as a build target."""
    name = func.__name__
    @functools.wraps(func)
    def wrapper() -> None:
        print(f'building target {name}', flush=True)
        func()
    _targets[name] = wrapper
    return wrapper

@target
def default() -> None:
    """Builds all targets."""
    buildifier()
    nogo()
    docs()
    # Test both default toolchain and versioned toolchains.
    check()
    versions()
    ext()

@target
def buildifier() -> None:
    """Checks that all BUILD files are formatted correctly."""
    _bazel('run',
           ['@com_github_bazelbuild_buildtools//buildifier',
            '--mode=check', '--lint=warn', '-r', '--', os.getcwd()])

@target
def nogo() -> None:
    """Checks that there are no unwanted Go targets in public packages."""
    print('looking for unwanted Go targets in public packages', flush=True)
    cwd = pathlib.Path(os.getcwd())
    for directory in ('elisp', 'emacs'):
        for dirpath, _, filenames in os.walk(cwd / directory):
            for file in filenames:
                file = pathlib.Path(dirpath) / file
                text = file.read_text(encoding='utf-8')
                if '@io_bazel_rules_go' in text:
                    raise ValueError('unwanted Go targets in {file} found')

@target
def check() -> None:
    """Runs the Bazel tests."""
    _check()

@target
def versions() -> None:
    """Runs the Bazel tests for all supported Emacs versions."""
    for version in sorted(_versions()):
        _check(f'--extra_toolchains=//elisp:emacs_{version}_toolchain')

def _check(*args: str) -> None:
    _bazel('test', ['//...'], options=['--test_output=errors'],
           postfix_options=args)

@target
def ext() -> None:
    """Run the tests in the example workspace."""
    _bazel('test', ['//...'], options=['--test_output=errors'],
           cwd=pathlib.Path(os.getcwd()) / 'examples' / 'ext')

@target
def docs() -> None:
    """Build the generated documentation files."""
    output = _run(
        ['bazel', 'query', '--output=label',
         r'filter("\.md\.generated$", kind("generated file", //...:*))'],
        capture_stdout=True)
    targets = output.splitlines()
    cwd = pathlib.Path(os.getcwd())
    bazel_bin = cwd / 'bazel-bin'
    _bazel('build', targets)
    for tgt in targets:
        gen = bazel_bin / tgt.lstrip('/').replace(':', os.sep)
        src = cwd / gen.with_suffix('').relative_to(bazel_bin)
        shutil.copyfile(gen, src)

def _bazel(command: str, targets: Iterable[str], *,
           options: Iterable[str] = (), postfix_options: Iterable[str] = (),
           cwd: Optional[pathlib.Path] = None) -> None:
    kernel = platform.system()
    args = ['bazel', command]
    args.extend(options)
    if kernel == 'Windows':
        # We only support compilation using MinGW-64 at the moment.  Binaries
        # linked with the MinGW-64 linker will depend on a few libraries that
        # reside in C:\msys64\mingw64\bin, e.g., libwinpthread-1.dll or
        # libstdc++-6.dll.  These libraries won’t be found because
        # C:\msys64\mingw64\bin is normally not in the search path.  We
        # therefore enforce static linking.  We need to add the -static option
        # to both the target and host options.  We need to add it to the
        # compiler option as well, otherwise the option comes too late, after
        # the -l… options.  Also see https://stackoverflow.com/a/14033674/.
        args += ['--compiler=mingw-gcc',
                 '--linkopt=-static', '--host_linkopt=-static',
                 '--copt=-static', '--host_copt=-static']
    args.extend(postfix_options)
    args.append('--')
    args.extend(targets)
    env = dict(os.environ)
    if env.get('CI') == 'true':
        # Hacks so that Bazel finds the right binaries on GitHub.  See
        # https://docs.github.com/en/actions/learn-github-actions/environment-variables#default-environment-variables.
        if kernel in ('Linux', 'Darwin'):
            tool_cache = pathlib.Path(os.getenv('RUNNER_TOOL_CACHE'))
            # Remove Python versions set up by the setup-python action.  For
            # some reason they don’t work.
            for var in ('PATH', 'LD_LIBRARY_PATH'):
                path = env.get(var, '').split(os.pathsep)
                prefix = str(tool_cache / 'Python') + '/'
                env[var] = os.pathsep.join(
                    d for d in path if not d.startswith(prefix))
        elif kernel == 'Windows':
            env['BAZEL_SH'] = r'C:\msys64\usr\bin\bash.exe'
    _run(args, cwd=cwd, env=env)

def _run(args: Sequence[str], *,
         cwd: Optional[pathlib.Path] = None,
         env: Optional[Mapping[str, str]] = None,
         capture_stdout: bool = False) -> Optional[str]:
    print(*map(shlex.quote, args), flush=True)
    result = subprocess.run(args, check=True, cwd=cwd, env=env,
                            stdout=subprocess.PIPE if capture_stdout else None,
                            encoding='utf-8')
    return result.stdout

def _versions() -> FrozenSet[str]:
    # All potentially supported Emacs versions.
    ret = {'26.1', '26.2', '26.3', '27.1', '27.2'}
    uname = platform.uname()
    if uname.system == 'Linux':
        # GNU/Linux supports all Emacs versions.
        pass
    elif uname.system == 'Darwin':
        # macOS only supports Emacs 27.
        ret -= {'26.1', '26.2', '26.3'}
        if uname.machine != 'x86_64':
            # Apple Silicon doesn’t support Emacs 27.1.
            ret.remove('27.1')
    elif uname.system == 'Windows':
        # Windows only supports Emacs 27.
        ret -= {'26.1', '26.2', '26.3'}
    else:
        raise ValueError(f'unsupported kernel {uname.system}')
    return frozenset(ret)

def main() -> None:
    """Builds the project."""
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('goals', nargs='*', choices=sorted(_targets))
    args = parser.parse_args(sys.argv[1:] or ['default'])
    try:
        build(args.goals)
    except subprocess.CalledProcessError as ex:
        print(*map(shlex.quote, ex.cmd), 'failed with exit code', ex.returncode,
              flush=True)
        sys.exit(ex.returncode)

if __name__ == '__main__':
    main()
