
#pragma once

#include "base.hpp"

namespace Fin {

using Alloc_Func   = void * (*) (void *, usize size, usize alignment);
using Free_Func    = void (*) (void *, void *);
using Realloc_Func = void * (*) (void *, void *, usize new_size, usize new_alignment);

struct Allocator {
  Alloc_Func   alloc;
  Free_Func    free;
  Realloc_Func realloc;

  void *context;
};

static void * alloc (Allocator &allocator, usize size, usize alignment = alignof(void *)) {
  return allocator.alloc(allocator.context, size, alignment);  
}

static void free (Allocator &allocator, void *value) {
  allocator.free(allocator.context, value);
}
  
static void * realloc (Allocator &allocator, void *value, usize new_size, usize new_alignment = alignof(void *)) {
  return allocator.realloc(allocator.context, value, new_size, new_alignment);
}
  
}
