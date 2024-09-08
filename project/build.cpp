
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

    size_t size = 0;
    if (run_system_command(p, command.c_str(), buffer, buffer_size, &size)) {
      fprintf(stderr, "Couldn't execute 'llvm-config --libdir' due to an error: %s\n", buffer);
      exit(1);
    }

    libs_folder = std::string(buffer, buffer[size - 1] == '\n' ? size - 1 : size);
  }

  LLVM_Config config;

  {
    auto command = std::string(llvm_config) + " --libnames " + libs;

    size_t size = 0;
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

    size_t size = 0;
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
  auto llvm_config = llvm_config_libs(project, "core x86");

  add_global_compiler_option(project, "-std:c++20 /EHa /MD /nologo /Z7 /Od");
  add_global_linker_options(project, "/debug:full /nologo");
  add_global_include_search_paths(project, llvm_config.include_path.c_str());

  auto target = add_executable(project, "main");
  {
    add_source_file(target, "code/main.cpp");
    add_compiler_option(target, "-DPLATFORM_WIN32");
    //add_compiler_options(target, "-O0 -g -gcodeview");

    for (auto &lib: llvm_config.lib_paths) link_with(target, lib.c_str());
    link_with(target, "psapi.lib shell32.lib ole32.lib uuid.lib advapi32.lib ws2_32.lib");
  }

  return true;
}
