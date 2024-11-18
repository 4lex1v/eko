
#pragma once

#include "anyfin/arena.hpp"
#include "anyfin/array.hpp"
#include "anyfin/base.hpp"
#include "anyfin/list.hpp"

namespace Fin {

template <typename T>
struct Seq {
  using Value_Type = T;

  struct Block {
    Array<T> block;
    usize    idx;
  };

  Memory_Arena &arena;

  List<Block>  blocks { arena };
  Block *head    = &blocks[0];
  Block *current = head;
  
  const usize block_capacity;
  usize count = 0;

  fin_forceinline
  constexpr Seq (Memory_Arena &_arena, usize _block_size = 64):
    arena          { _arena },
    block_capacity { _block_size }
  {}

  // fin_forceinline
  // constexpr decltype(auto) operator [] (this auto &&self, usize offset) {
  //   fin_ensure(offset < self.capacity);
  //   return self.values[offset];
  // }

  void grow () {
    fin_ensure(this->count == this->capacity);
    this->current = &list_push(this->blocks, {});
  }

};

template <typename T>
static void seq_push (Seq<T> &seq, typename Seq<T>::Value_Type &&value) {
  if (seq.needs_growth()) [[unlikely]] seq.grow();
  fin_ensure(seq.current.idx < seq.block_size);

  seq.current.block[seq.current.idx++] = move(value);
}

template <typename T>
static void seq_push_copy (Seq<T> &seq, const typename Seq<T>::Value_Type &value) {
  using VT = typename Seq<T>::Value_Type;
  set_push(seq, V(value));
}

template <typename T>
static decltype(auto) seq_get (Seq<T> &seq, usize offset) {
  if (offset > seq.count) return nullptr;

  usize block_index = offset / seq.block_size;
}

// template <typename T>
// static Seq<T> reserve_seq (Memory_Arena &arena, usize count, usize alignment = alignof(T)) {
//   if (count == 0) return {};

//   auto memory = reserve<T>(arena, count * sizeof(T), alignment);
//   fin_ensure(memory);

//   if (!memory) return {};

//   return Seq(memory, count);
// }

}
