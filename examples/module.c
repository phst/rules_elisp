// Copyright 2020, 2022 Google LLC
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

#include <stddef.h>
#include <string.h>

#include "emacs-module.h"

static emacs_value ModuleFunc(emacs_env* env, ptrdiff_t nargs,
                              emacs_value* args, void* data) {
  const char* message = "hi from module";
  return env->make_string(env, message, strlen(message));
}

typedef emacs_value (*Function)(emacs_env*, ptrdiff_t, emacs_value*, void*);

static void Defun(emacs_env* env, const char* name, ptrdiff_t min_arity,
                  ptrdiff_t max_arity, Function func) {
  emacs_value args[] = {
      env->intern(env, name),
      env->make_function(env, min_arity, max_arity, func, NULL, NULL),
  };
  env->funcall(env, env->intern(env, "defalias"), 2, args);
}

static void Provide(emacs_env* env, const char* feature) {
  emacs_value symbol = env->intern(env, feature);
  env->funcall(env, env->intern(env, "provide"), 1, &symbol);
}

int emacs_module_init(struct emacs_runtime* rt) {
  if (rt->size < sizeof *rt) return 1;
  emacs_env* env = rt->get_environment(rt);
  if (env->size < sizeof(struct emacs_env_26)) return 2;
  Defun(env, "module-func", 0, 0, ModuleFunc);
  Provide(env, "examples/module");
  return 0;
}

int plugin_is_GPL_compatible = 1;
