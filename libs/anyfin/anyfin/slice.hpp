
#pragma once

#include "anyfin/base.hpp"

namespace Fin {

template <typename T>
struct Slice {
  T *values = nullptr;
  usize count = 0;

  fin_force_inline constexpr Slice () = default;

  template <usize N>
  fin_force_inline constexpr Slice (T (&data)[N])
    : Slice(data, N) {}
  
  fin_force_inline constexpr Slice (T *_value, usize _count)
    : values { _value }, count { _count } {}

  fin_force_inline constexpr operator bool (this auto self) { return self.values && self.count; }

  fin_force_inline constexpr decltype(auto) operator [] (this auto &&self, usize offset) { return self.value[offset]; }
  fin_force_inline constexpr decltype(auto) operator *  (this auto &&self)               { return *self.values; }

  fin_force_inline
  constexpr Slice<T> operator + (this auto self, usize offset) {
    assert(offset <= self.count);
    return Slice(self.values + offset, self.count - offset);
  }

  fin_force_inline
  constexpr Slice<T>& operator += (this Slice<T> &self, usize offset) {
    fin_ensure(offset <= self.count);

    self.values += offset;
    self.count  -= offset;

    return self;
  }

  fin_force_inline constexpr Slice<T> operator ++ (this Slice<T> &self, int) { return (self += 1); }

  fin_force_inline constexpr decltype(auto) begin (this auto self) { return self.values; }
  fin_force_inline constexpr decltype(auto) end   (this auto self) { return self.values + self.count; }
};

template <typename T>
constexpr bool is_empty (Slice<T> args) {
  return args.count == 0;
}

}
