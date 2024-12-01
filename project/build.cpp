
#include "cbuild.h"
#include "cbuild_experimental.h"

#ifdef __cplusplus
extern "C"
#endif
bool setup_project (const Arguments *args, Project *project) {
  set_toolchain(project, Toolchain_Type_LLVM);

  add_global_include_search_path(project, "libs");
  add_global_compiler_option(project, "-std=c++2b -DDEV_BUILD");
  add_global_linker_options(project, "/debug:full /nologo /subsystem:console");

  auto target = add_executable(project, "eko");
  {
    add_include_search_path(target, "libs/llvm/include");

    add_source_file(target, "code/driver.cpp");
    add_source_file(target, "code/tokenizer.cpp");
    add_source_file(target, "code/parser.cpp");
    add_source_file(target, "code/brain.cpp");
    add_source_file(target, "code/llvm_codegen.cpp");

    add_compiler_option(target, "-DPLATFORM_WIN32");
    add_compiler_options(target, "-O0 -g -gcodeview");

    link_with(target, "msvcrtd.lib", "kernel32.lib", "llvm-c.lib");
  }

  return true;
}
