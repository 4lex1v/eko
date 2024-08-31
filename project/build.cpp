
#include "cbuild.h"
#include "cbuild_experimental.h"

#include <cstdio>
#include <array>
#include <string>

static const char * llvm_config_libs (Project *p, const char *name) {
  char command[1024] = "C:\\Program Files (x86)\\LLVM\\bin\\llvm-config.exe --libs ";
  strcat(command, name);

  enum { buffer_size = 1024 };
  auto buffer = (char*) calloc(buffer_size, sizeof(char));
  if (run_system_command(p, command, buffer, buffer_size, nullptr)) {
    fprintf(stderr, "Couldn't resolve LLVM libraries due to an error: %s\n", buffer);
    exit(1);
  }
  
  return buffer;
}

#ifdef __cplusplus
extern "C"
#endif
bool setup_project (const Arguments *args, Project *project) {
  set_toolchain(project, Toolchain_Type_LLVM);

  add_global_compiler_option(project, "-std=c++23 -fno-exceptions");
  add_global_linker_options(project, "/debug:full");
  add_global_include_search_paths(project, "libs/anyfin", "libs/llvm");

  auto target = add_executable(project, "main");
  {
    add_source_file(target, "code/main.cpp");
    add_compiler_option(target, "-D_CRT_SECURE_NO_WARNINGS -DPLATFORM_WIN32 -fms-runtime-lib=libcmt_dll");
    add_compiler_options(target, "-O0 -g -gcodeview");

    link_with(target, llvm_config_libs(project, "core"));
  }

  return true;
}
