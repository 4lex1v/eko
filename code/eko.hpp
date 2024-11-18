
#pragma once

#include "anyfin/base.hpp"
#include "anyfin/arena.hpp"
#include "anyfin/format.hpp"
#include "anyfin/platform.hpp"
#include "anyfin/console.hpp"

using namespace Fin;

using Panic_Handler = void (*)(u32);
extern Panic_Handler panic_handler;

template <usize MEMORY_SIZE = 1024, String_Convertible... Args>
static void log (Format_String &&str_format, Args&&... args) {
  u8 stack_memory[MEMORY_SIZE];
  Memory_Arena arena { stack_memory };

  write_to_stdout(format_string(arena, move(str_format), forward<Args>(args)...));
}

template <usize MEMORY_SIZE = 1024, String_Convertible... Args> 
[[noreturn]] static void panic (Format_String &&format, Args&&... args) {
  log(move(format), forward<Args>(args)...);

  panic_handler(1);
  __builtin_unreachable();
}

template <typename T>
static T && unwrap (Sys_Result<T> &&result, Callsite callsite = {}) {
  if (result.is_ok()) [[likely]] return move(result.value);
  panic("% - ERROR: Call failed due to the error: %\n", callsite, result.error.value);
}
