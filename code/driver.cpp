
#include "anyfin/file_system.hpp"
#include "anyfin/c_runtime_compat.hpp"
#include "anyfin/process.hpp"
#include "anyfin/arena.hpp"
#include "anyfin/format.hpp"
#include "anyfin/platform.hpp"
#include "anyfin/console.hpp"

#include "eko.hpp"
#include "codegen.hpp"
#include "parser.hpp"
#include "tokenizer.hpp"
#include "typer.hpp"

using Fin::Memory_Arena;

constexpr usize memory_reservation = Fin::megabytes(64);

using Panic_Handler = void (*)(u32);
Panic_Handler panic_handler = Fin::terminate;

template <usize MEMORY_SIZE = 1024, Fin::String_Convertible... Args>
static void log (Fin::Format_String &&str_format, Args&&... args) {
  u8 stack_memory[MEMORY_SIZE];
  Fin::Memory_Arena arena { stack_memory };

  write_to_stdout(format_string(arena, Fin::move(str_format), Fin::forward<Args>(args)...));
}

template <usize MEMORY_SIZE = 1024, Fin::String_Convertible... Args> 
[[noreturn]] static void panic (Fin::Format_String &&format, Args&&... args) {
  log(move(format), forward<Args>(args)...);

  panic_handler(1);
  __builtin_unreachable();
}

template <typename T>
static T && unwrap (Fin::Sys_Result<T> &&result, Fin::Callsite callsite = {}) {
  if (result.is_ok()) [[likely]] return result.value.take();
  panic("% - ERROR: Call failed due to the error: %\n", callsite, result.error.take());
}

static Source_File create (Memory_Arena arena, const Module_Context *context, const char *file_path) {
  auto sample_file = unwrap(Fin::open_file(file_path));
  auto file_size   = unwrap(get_file_size(sample_file));
  if (file_size == 0) {
    log("Attempt to read empty file: %. Program will be terminated\n", file_path);
    exit(1);
  }

  auto file_buffer = reserve(arena, file_size + 1);
  read_bytes_into_buffer(sample_file, file_buffer, file_size);
  file_buffer[file_size] = '\0';
  
  return Source_File(arena, context, file_path, Fin::Array(reinterpret_cast<char *>(file_buffer), file_size + 1));
}

int main () {
  auto arena = Memory_Arena { Fin::reserve_virtual_memory(memory_reservation) };

  auto working_directory_path = unwrap(get_working_directory(arena));
  log("Working directory: %\n", working_directory_path);

  auto file_path   = "samples/main.eko";

  Scope global_scope (arena);
  Module_Context module (Scope(arena, &global_scope));

  auto unit = create(arena, &module, "samples/main.eko");

  auto tokenizer_status = read_tokens(arena, unit);
  if (tokenizer_status != Tokenizer_Status::Success) return 1;

  auto tree = build_tree(arena, unit);

  return 0;
}
