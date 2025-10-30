# Copyright 2025 Philipp Stephani
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

"""Helper functions to create C/C++ literals from Starlark values."""

visibility(["//elisp"])

def cc_bool(value):
    return "true" if value else "false"

def cc_int(value):
    return str(value)

def cc_ints(list):
    return ", ".join([cc_int(i) for i in list])
