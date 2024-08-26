
#include "cbuild.h"

#include <stdio.h>
#include <string.h>

#ifdef __cplusplus
extern "C"
#endif
bool setup_project (const Arguments *args, Project *project) {
  set_toolchain(project, Toolchain_Type_LLVM);

  add_global_compiler_option(project, "-std=c++23 -fno-exceptions");
  add_global_linker_options(project, "/debug:full");
  add_global_include_search_path(project, "libs/anyfin");

  auto target = add_executable(project, "main");
  {
    add_source_file(target, "code/main.cpp");
    add_compiler_option(target, "-D_CRT_SECURE_NO_WARNINGS -DPLATFORM_WIN32 -fms-runtime-lib=libcmt_dll");
    add_compiler_options(target, "-O0 -g -gcodeview");
  }

  return true;
}
