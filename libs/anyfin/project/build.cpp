
#include <cstring>

#include "cbuild.h"

extern "C" bool setup_project (const Arguments *args, Project *project) {
  auto debug_mode = get_argument_or_default(args, "debug_mode", "on");

  add_global_compiler_options(project,
                              "-std=c++20",
                              "-O0 -g -gcodeview",
                              "-DPLATFORM_WIN32 -DCPU_ARCH_X64",
                              "-DRHI_OPENGL");

  add_global_compiler_option(project, "-DDEV_TOOLS_ENABLED");

  add_global_include_search_path(project, ".");

  set_toolchain(project, Toolchain_Type_LLVM);

  auto fonts_freetype = add_static_library(project, "freetype");
  {
    add_compiler_option(fonts_freetype, "-DFT2_BUILD_LIBRARY");
    add_include_search_path(fonts_freetype, "anyfin/ui/fonts/freetype/include");
    add_source_files(fonts_freetype,
                     "anyfin/ui/fonts/freetype/src/base/ftsystem.c", 
                     "anyfin/ui/fonts/freetype/src/base/ftinit.c", 
                     "anyfin/ui/fonts/freetype/src/base/ftbase.c", 
                     "anyfin/ui/fonts/freetype/src/base/ftbitmap.c", 
                     "anyfin/ui/fonts/freetype/src/sfnt/sfnt.c", 
                     "anyfin/ui/fonts/freetype/src/gzip/ftgzip.c", 
                     "anyfin/ui/fonts/freetype/src/psnames/psnames.c", 
                     "anyfin/ui/fonts/freetype/src/truetype/truetype.c", 
                     "anyfin/ui/fonts/freetype/src/smooth/smooth.c", 
                     "anyfin/ui/fonts/freetype/src/autofit/autofit.c");

    if (strstr(debug_mode, "on")) {
      add_source_file(fonts_freetype, "anyfin/ui/fonts/freetype/src/base/ftdebug.c");
      add_compiler_option(fonts_freetype, "-DFT_CONFIG_OPTION_ERROR_STRINGS -DFT_DEBUG_LEVEL_ERROR");
    }
  }

  auto ui_fonts = add_static_library(project, "fonts");
  {
    
  }

  auto opengl_rhi = add_static_library(project, "opengl_rhi");
  {
    add_source_file(opengl_rhi, "anyfin/rhi/opengl/win32/opengl_loader_win32.cpp");
    add_source_file(opengl_rhi, "anyfin/rhi/opengl/opengl.cpp");
    add_compiler_options(opengl_rhi, "-fno-exceptions");
  }

  auto window = add_static_library(project, "window");
  {
    add_source_file(window, "anyfin/window/win32/window_win32.cpp");
    add_compiler_options(window, "-fno-exceptions");
  }

  auto platform = add_static_library(project, "platform");
  {
    add_all_sources_from_directory(platform, "anyfin/platform/win32", "cpp", false);
    add_compiler_options(platform, "-fno-exceptions");
  }

  auto fury = add_executable(project, "fury");
  {
    add_source_file(fury, "demos/squares_of_fury/game.cpp");
    add_compiler_options(fury, "-fno-exceptions");
    add_linker_option(fury, "/debug:full /subsystem:Windows");
    link_with(fury,
              window,
              opengl_rhi, "gdi32.lib", "opengl32.lib",
              platform,"kernel32.lib user32.lib");
  }

  auto snake = add_executable(project, "snake");
  {
    add_source_file(snake, "demos/snake/code/game.cpp");
    add_compiler_options(snake, "-fno-exceptions");
    add_linker_option(snake, "/debug:full /subsystem:Windows");
    link_with(snake,
              window,
              opengl_rhi, "gdi32.lib", "opengl32.lib",
              platform, "kernel32.lib user32.lib advapi32.lib");
  }


  return true;
}
