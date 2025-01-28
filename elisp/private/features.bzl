# Copyright 2021, 2022, 2023, 2024, 2025 Google LLC
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

"""Defines internal functions to deal with feature strings."""

visibility("private")

def parse_features(features):
    """Parse a list of feature strings.

    Args:
      features (list of str): feature strings from some `features` attribute

    Returns:
      a pair (features, disabled_features) of lists of strings
    """
    return (
        [f for f in features if not f.startswith("-")],
        [f.removeprefix("-") for f in features if f.startswith("-")],
    )
