
#pragma once

#include "base.hpp"

namespace Fin {

using Alloc_Func   = void * (*) (usize size, usize alignment);
using Free_Func    = void (*) (void *);
using Realloc_Func = void * (void *, usize new_size, usize new_alignment);

struct Allocator {
  Alloc_Func   alloc;
  Free_Func    free;
  Realloc_Func realloc;

  void *context;
};

static inline void * alloc (Allocator &allocator, usize size, usize alignment = alignof(void *)) {
  return allocator.alloc(size, alignment);  
}

static inline void free (Allocator &allocator, void *value) {
  allocator.free(value);
}
  
static inline void * realloc (Allocator &allocator, void *value, usize new_size, usize new_alignment = alignof(void *)) {
  return allocator.realloc(value, new_size, new_alignment);
}
  
}
