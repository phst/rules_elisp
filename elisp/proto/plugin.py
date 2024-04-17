# Copyright 2024 Philipp Stephani
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

"""Protocol buffer compiler plugin for Emacs Lisp.

See https://protobuf.dev/reference/other/."""

import pathlib
import shutil
import subprocess
import sys
import tempfile

from google.protobuf import descriptor_database
from google.protobuf.compiler import plugin_pb2

def main() -> None:
    """Entry point."""
    request = plugin_pb2.CodeGeneratorRequest.FromString(  # pytype: disable=module-attr pylint: disable=no-member
        sys.stdin.buffer.read())
    response = plugin_pb2.CodeGeneratorResponse()  # pytype: disable=module-attr pylint: disable=no-member
    generate = pathlib.Path(request.parameter)
    database = descriptor_database.DescriptorDatabase()
    for file in request.proto_file:
        database.Add(file)
    tempdir = pathlib.Path(tempfile.mkdtemp())
    infile = tempdir / 'in.binpb'
    outfile = tempdir / 'out.el'
    for name in request.file_to_generate:
        descriptor = database.FindFileByName(name)
        # Strip unnecessary documentation.
        descriptor.ClearField('source_code_info')
        infile.write_bytes(descriptor.SerializeToString(deterministic=True))
        subprocess.run([generate, infile, outfile, name], check=True)
        response.file.add(name=name + '.el',
                          content=outfile.read_text(encoding='utf-8'))
    sys.stdout.buffer.write(response.SerializeToString(deterministic=True))
    shutil.rmtree(tempdir)


if __name__ == '__main__':
    main()
