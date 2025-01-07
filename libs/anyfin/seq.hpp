
#pragma once

#include "anyfin/allocator.hpp"
#include "anyfin/array.hpp"
#include "anyfin/base.hpp"
#include "anyfin/list.hpp"

namespace Fin {

template <typename T>
struct Seq {
  using Value_Type = T;

  Allocator allocator;
  T *memory = nullptr;

  usize count    = 0;
  usize capacity = 0;

  constexpr Seq () {}

  fin_forceinline constexpr Seq (Allocator _allocator): allocator { _allocator } {}

  fin_forceinline constexpr Seq (Allocator _allocator, usize default_capacity = 16):
    allocator { _allocator }, capacity { default_capacity } {}

  fin_forceinline
  constexpr decltype(auto) operator [] (this auto &&self, usize offset) {
    fin_ensure(offset < self.capacity);
    return self.values[offset];
  }

  bool needs_growth () const {
    return this->count >= this->capacity;
  }

  void grow () {
    fin_ensure(this->count == this->capacity);
    this->current = &list_push(this->blocks, {});
  }

};

template <typename T>
static void seq_push (Seq<T> &seq, typename Seq<T>::Value_Type &&value) {
  if (seq.needs_growth()) [[unlikely]] seq.grow();
  seq.memory[seq.count++] = move(value);
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
