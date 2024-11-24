
#pragma once

#include "anyfin/base.hpp"
#include "meta.hpp"

namespace Fin {

template <typename T>
struct Uninit {
  using Value_Type = T;

  union {
    T value;
    char _bytes[sizeof(T)];
  };

  Uninit () {}
  
  Uninit (const T &value): value { value }            {};
  Uninit (T &&value):      value { Fin::move(value) } {};

  fin_forceinline
  const T & get() const { return this->value; }

  fin_forceinline
  T && take () { return Fin::move(this->value); }

  fin_forceinline
  T * operator -> (this auto &&self) { return &self.value; }
};

}
