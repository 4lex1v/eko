
#include "anyfin/file_system.hpp"
#include "anyfin/c_runtime_compat.hpp"
#include "anyfin/process.hpp"

#include "eko.hpp"
#include "codegen.hpp"
#include "parser.hpp"
#include "typer.hpp"

using namespace Eko;

constexpr usize memory_reservation = megabytes(64);

Panic_Handler Eko::panic_handler = terminate;

int mainCRTStartup () {
  auto arena = Memory_Arena { reserve_virtual_memory(memory_reservation) };

  auto working_directory_path = unwrap(get_working_directory(arena));
  log("Working directory: %\n", working_directory_path);

  auto file_path   = "samples/main.eko";
  auto sample_file = unwrap(open_file(file_path));
  auto file_size   = unwrap(get_file_size(sample_file));
  if (file_size == 0) {
    log("Attempt to read empty file: %. Program will be terminated\n", file_path);
    return 0;
  }

  auto file_buffer = reserve(arena, file_size + 1);
  read_bytes_into_buffer(sample_file, file_buffer, file_size);
  file_buffer[file_size] = '\0';
  
  auto tree = build_tree(reinterpret_cast<const char *>(file_buffer));
  // typecheck(tree);
  
  codegen();

  return 0;
}
