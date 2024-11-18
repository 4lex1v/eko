
#include "cbuild.h"
#include "cbuild_experimental.h"

#include <cstdio>
#include <string>
#include <sstream>
#include <vector>

struct LLVM_Config {
  std::string include_path;
  std::vector<std::string> lib_paths;
};

std::vector<std::string> split_string(const std::string& str) {
  std::vector<std::string> result;
  std::string temp;
    
  for (char ch : str) {
    if (ch == ' ') {
      if (!temp.empty()) {
        result.push_back(temp);
        temp.clear();
      }
    } else {
      temp += ch;
    }
  }
    
  if (!temp.empty()) {
    result.push_back(temp);
  }
    
  return result;
}

static LLVM_Config llvm_config_libs (Project *p, const char *libs) {
  auto llvm_config = find_executable(p, "llvm-config");
  if (!llvm_config) {
    fprintf(stderr, "llvm-config is not found on the host machine\n");
    exit(1);
  }

  enum { buffer_size = 1024 };
  char buffer[buffer_size];

  std::string libs_folder;
  {
    auto command = std::string(llvm_config) + " --libdir";  

    unsigned size = 0;
    if (run_system_command(p, command.c_str(), buffer, buffer_size, &size)) {
      fprintf(stderr, "Couldn't execute 'llvm-config --libdir' due to an error: %s\n", buffer);
      exit(1);
    }

    libs_folder = std::string(buffer, buffer[size - 1] == '\n' ? size - 1 : size);
  }

  LLVM_Config config;

  {
    auto command = std::string(llvm_config) + " --libnames " + libs;

    unsigned size = 0;
    if (run_system_command(p, command.c_str(), buffer, buffer_size, &size)) {
      fprintf(stderr, "Couldn't execute '%s' due to an error: %s\n", command.c_str(), buffer);
      exit(1);
    }

    auto libs = split_string(std::string(buffer, buffer[size - 1] == '\n' ? size - 1 : size));

    for (auto &lib: libs) {
      std::string lib_path = "\"";
      lib_path += libs_folder;
      lib_path += "\\";
      lib_path += lib;
      lib_path += "\"";
      
      config.lib_paths.push_back(lib_path);
    }
  }

  {
    auto command = std::string(llvm_config) + " --includedir";

    unsigned size = 0;
    if (run_system_command(p, command.c_str(), buffer, buffer_size, &size)) {
      fprintf(stderr, "Couldn't execute '%s' due to an error: %s\n", command.c_str(), buffer);
      exit(1);
    }

    config.include_path = std::string(buffer, buffer[size - 1] == '\n' ? size - 1 : size);
  }
  
  return config;
}

#ifdef __cplusplus
extern "C"
#endif
bool setup_project (const Arguments *args, Project *project) {
  set_toolchain(project, Toolchain_Type_LLVM);

  auto llvm_config = llvm_config_libs(project, "core x86");

  add_global_system_include_search_path(project, llvm_config.include_path.c_str());

  add_global_include_search_path(project, "libs");
  add_global_compiler_option(project, "-std=c++2b");
  add_global_linker_options(project, "/debug:full /nologo /subsystem:console");

  auto target = add_executable(project, "eko");
  {

    add_source_files(target,
                     "code/driver.cpp",
                     "code/parser.cpp",
                     "code/typer.cpp",
                     "code/llvm_codegen.cpp");
    add_compiler_option(target, "-DPLATFORM_WIN32");
    //add_compiler_options(target, "-O0 -g -gcodeview");

    // for (auto &lib: llvm_config.lib_paths) link_with(target, lib.c_str());
    link_with(target, "kernel32.lib");
  }

  return true;
}
