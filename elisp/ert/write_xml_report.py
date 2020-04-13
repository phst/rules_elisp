# Copyright 2020 Google LLC
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

import argparse
import json
import pathlib
import xml.etree.ElementTree as etree


def main() -> None:
    """Writes an XML report for bazel test."""
    parser = argparse.ArgumentParser()
    parser.add_argument('json', type=pathlib.Path)
    parser.add_argument('xml', type=pathlib.Path)
    args = parser.parse_args()
    report = json.loads(args.json.read_text('utf-8'))
    elapsed = report['elapsed']
    total = len(report['tests'])
    # The expected format of the XML output file isn’t well-documented.
    # https://docs.bazel.build/versions/3.0.0/test-encyclopedia.html#initial-conditions
    # only states that the XML file is “ANT-like.”
    # https://llg.cubic.org/docs/junit/ and
    # https://help.catchsoftware.com/display/ET/JUnit+Format contain a bit of
    # documentation.
    root = etree.Element('testsuites', tests=str(total), time=str(elapsed))
    suite = etree.SubElement(root, 'testsuite', id='0', tests=str(total),
                             time=str(elapsed), timestamp=report['start-time'])
    failures = 0
    errors = 0
    for test in report['tests']:
        case = etree.SubElement(
            suite, 'testcase', name=test['name'], time=str(test['elapsed']))
        if test['expected']:
            continue
        message = test['message']
        status = test['status']
        if status.lower() == 'failed':
            failure = etree.SubElement(case, 'failure')
            failure.text = message
            failures += 1
        else:
            error = etree.SubElement(case, 'error', type=status)
            error.text = message
            errors += 1
    root.set('failures', str(failures))
    suite.set('failures', str(failures))
    suite.set('errors', str(errors))
    etree.ElementTree(root).write(args.xml)


if __name__ == '__main__':
    main()
