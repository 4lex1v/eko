
#define FIN_MEMORY_HPP_IMPL

#include "anyfin/win32.hpp"

#include "anyfin/memory.hpp"

namespace Fin {

static Memory_Region reserve_virtual_memory (usize size) {
  SYSTEM_INFO system_info;
  GetSystemInfo(&system_info);
  
  const auto aligned_size = align_forward(size, system_info.dwPageSize);

  auto memory = VirtualAlloc(nullptr, aligned_size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);

  return Memory_Region { static_cast<u8*>(memory), aligned_size };
}

static void free_virtual_memory (Memory_Region &region) {
  VirtualFree(region.memory, region.size, MEM_RELEASE);
}

}
