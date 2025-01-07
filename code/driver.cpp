
#include "anyfin/file_system.hpp"
#include "anyfin/c_runtime_compat.hpp"
#include "anyfin/process.hpp"
#include "anyfin/arena.hpp"
#include "anyfin/format.hpp"
#include "anyfin/platform.hpp"
#include "anyfin/console.hpp"

#include "eko.hpp"
#include "brain.hpp"
#include "codegen.hpp"
#include "parser.hpp"
#include "tokenizer.hpp"

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

static Source_File create (Fin::Memory_Arena &arena, Scope &global, const char *file_path) {
  auto sample_file = unwrap(Fin::open_file(file_path));
  auto file_size   = unwrap(get_file_size(sample_file));
  if (file_size == 0) {
    log("Attempt to read empty file: %. Program will be terminated\n", file_path);
    exit(1);
  }

  auto file_buffer = reserve(arena, file_size + 1);
  read_bytes_into_buffer(sample_file, file_buffer, file_size);
  file_buffer[file_size] = '\0';
  
  return Source_File(arena, global, file_path, Fin::Array(reinterpret_cast<char *>(file_buffer), file_size + 1));
}

Fin::Memory_Arena global_arena;

int main () {
  global_arena = Fin::Memory_Arena { Fin::reserve_virtual_memory(memory_reservation) };

  auto working_directory_path = unwrap(get_working_directory(global_arena));
  log("Working directory: %\n", working_directory_path);

  auto file_path   = "samples/main.eko";

  Scope global_scope(global_arena, nullptr, "Global");

  auto unit = create(global_arena, global_scope, "samples/main.eko");

  auto tokenizer_status = read_tokens(unit);
  if (tokenizer_status != Tokenizer_Status::Success) return 1;

  auto parser_error = build_tree(unit);
  if (parser_error) return 1;

  init_typer();
  auto typer_error = typecheck(unit);
  if (typer_error.is_error()) return 1;

  auto [codegen_error] = codegen(unit);
  if (codegen_error) return 1;

  return 0;
}
