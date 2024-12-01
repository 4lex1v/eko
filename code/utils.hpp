
#pragma once

#define try2(NAME, FUNC, CNT)                   \
  auto [tokenpaste(error_, CNT), tokenpaste(value_, CNT)] = (FUNC);         \
  if (tokenpaste(error_, CNT)) return tokenpaste(error_, CNT).take();       \
  auto NAME = tokenpaste(value_, CNT).take();

#define try(NAME, FUNC) try2(NAME, FUNC, __COUNTER__)

#define KIND_TAG(TYPE, KIND) static const auto kind = TYPE::KIND

#define GEN_CONSTRUCTOR(TYPE, KIND, FIELD)      \
  template <typename T>                         \
  TYPE (T value):                               \
    KIND { T::kind }                            \
  {                                             \
    new (&this->FIELD) T(Fin::move(value));     \
  } 
