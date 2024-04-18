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

from google.protobuf import descriptor_pb2  # pylint: disable=import-error
from google.protobuf.compiler import plugin_pb2  # pylint: disable=import-error

def main() -> None:
    """Entry point."""
    request = plugin_pb2.CodeGeneratorRequest.FromString(
        sys.stdin.buffer.read())
    response = plugin_pb2.CodeGeneratorResponse()
    generate = pathlib.Path(request.parameter)
    tempdir = pathlib.Path(tempfile.mkdtemp())
    infile = tempdir / 'in.binpb'
    outfile = tempdir / 'out.el'
    descriptors = descriptor_pb2.FileDescriptorSet()
    for file in request.proto_file:
        file.ClearField('source_code_info')  # strip unnecessary documentation
        descriptors.file.add().CopyFrom(file)
    infile.write_bytes(descriptors.SerializeToString(deterministic=True))
    for name in request.file_to_generate:
        subprocess.run([generate, infile, outfile, name], check=True)
        response.file.add(name=name + '.el',
                          content=outfile.read_text(encoding='utf-8'))
    sys.stdout.buffer.write(response.SerializeToString(deterministic=True))
    shutil.rmtree(tempdir)


if __name__ == '__main__':
    main()