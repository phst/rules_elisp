// Copyright 2026 Philipp Stephani
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

#include <stdio.h>
#include <stdlib.h>

#include <unistd.h>

#include <mach/mach.h>
#include <mach/port.h>
#include <mach/task.h>

int main(int argc, char** argv) {
  if (argc < 2) {
    fprintf(stderr, "Too few command-line arguments: %d\n", argc);
    abort();
  }
  const kern_return_t error = task_set_exception_ports(
      mach_task_self(), EXC_MASK_ALL | EXC_MASK_CRASH, MACH_PORT_NULL,
      EXCEPTION_DEFAULT, THREAD_STATE_NONE);
  if (error != KERN_SUCCESS) {
    fprintf(stderr, "task_set_exception_ports: error %#10x\n", error);
  }
  extern char** environ;
  execve(argv[1], argv + 1, environ);
  perror("execve");
  abort();
}
