
#pragma once

#include "base.hpp"
#include "memory.hpp"

namespace Fin {
  

struct Heap {
  struct Block {
    enum Use_Status {
      Free   = 0,
      In_Use = 1,
    };

    struct Header {
      usize in_use : 1;
      usize tag : 47;

      explicit Header(const Use_Status use_status = Free, const char* const tag_value = nullptr):
        in_use { static_cast<usize>(use_status) },
        tag    { reinterpret_cast<usize>(tag_value) }
      {}
    };

    static_assert(sizeof(Header) == sizeof(usize));

    Header header;
    usize  size;

    Block* next_block = nullptr;
    Block* next_free  = nullptr;

    explicit Block (usize block_size): size { block_size } {}

    bool is_free () const { return header.in_use == 0x0u; }
    bool is_used () const { return !is_free(); }

    u8* memory () { return reinterpret_cast<u8*>(this) + sizeof(Block); }

    const char* read_tag() const { return (const char*)header.tag; }
  };

  Memory_Region memory;
  
  Block *blocks      = nullptr;
  Block *free_blocks = nullptr;

  Heap () = default;
  explicit Heap (Memory_Region region);

  static const char* find_tag (void *ptr);
  
  u8*  alloc   (usize size, const char *tag);
  u8*  realloc (void *ptr, usize new_size, const char * tag);
  void free    (void *ptr, const char *tag);

  template <typename T> T* alloc (const char *tag) {
    return reinterpret_cast<T*>(this->alloc(sizeof(T), tag));
  }

  template <typename T, typename ...Args>
  T* emplace (const char *tag, Args&&... args) {
    auto ptr   = alloc(sizeof(T), tag);
    auto value = new(ptr) T(std::forward<Args>(args)...);
    return value;
  }

  void report_leaks () const;
  void destroy ();
};


static_assert(sizeof(Heap::Block) == 32);
static_assert(alignof(Heap::Block) == 8);

//constexpr usize Block_Min_Size = align_forward(sizeof(Heap::Block), default_alignment) + default_alignment;

static inline Heap::Block * get_memory_block (void * const ptr) {
  auto block = reinterpret_cast<Heap::Block *>(reinterpret_cast<u8 *>(ptr) - sizeof(Heap::Block));
  return block;
}

static inline Heap::Block * get_previous_free_block (Heap::Block * const free_list, const Heap::Block * const block) {
  Heap::Block * previous_free_block = nullptr;
  for (auto it = free_list; it != block; it = it->next_free) {
    previous_free_block = it;
  }
  
  return previous_free_block;
}

/*
  Partition size here is the upper bound of the first block after which the new partition starts. 
  Basically the block gets partitioned into two new blocks of sizes "partition_size" and block->size - partition_size 
  with alignment considered. The provided partition_alignment would be set as the alignment of the first partition.
*/
static Heap::Block * partition_block (Heap::Block * const block, const usize partition_size) {
  assert(partition_size + Block_Min_Size <= block->size);
  assert(is_aligned_by(partition_size, default_alignment));

  // Including the header size
  const usize new_block_total_size = block->size - partition_size;

  // partitioning the block should double check that it's 32 byte aligned.
  auto *new_block = reinterpret_cast<Heap::Block*>(block->memory() + partition_size);
  assert(is_aligned_by(new_block, default_alignment));

  memcpy_s(new_block, new_block_total_size, block, sizeof(Heap::Block));

  new_block->size   = new_block_total_size - sizeof(Heap::Block);
  new_block->header = Heap::Block::Header();

  block->size       = partition_size;
  block->next_block = new_block;
  block->next_free  = new_block;

  return new_block;
}

static bool merge_free_blocks (Heap::Block * const block) {
  bool merged = false;

  while (block->next_block) {
    if (block->next_block != block->next_free) {
      assert(block->next_block->is_used());
      break;
    }
    
    auto &next_block = *block->next_block;
    assert(next_block.is_free());
    
    block->size       += next_block.size + sizeof(Heap::Block);
    block->next_block  = next_block.next_block;
    block->next_free   = next_block.next_free;

    merged = true;
  }

  return merged;
}

Heap::Heap (Memory_Region region)
  : memory(region)
{
  assert(region.size >= (sizeof(Block) + sizeof(void *)));
  assert(is_aligned_by(region.memory, 8));

  auto *block = new(region.memory) Block(region.size - sizeof(Block));

  blocks = free_blocks = block;
}

void Heap::destroy () {
  memset(memory.memory, 0, memory.size);
  memory = Memory_Region();
  blocks = free_blocks = nullptr;
}

static inline void push_block_to_the_free_list (Heap::Block ** const free_list, Heap::Block * const block) {
  Heap::Block *prev_free = nullptr, *next_free = nullptr;
  for (auto it = *free_list; it; it = it->next_free) {
    if (block < it) {
      next_free = it;
      break;
    }

    prev_free = it;
  }

  block->next_free = next_free;
  if (prev_free != nullptr) {
    prev_free->next_free = block;
    if (prev_free->next_block == block) assert(merge_free_blocks(prev_free) == true);
    else                                merge_free_blocks(block);
  }
  else {
    *free_list = block;
    merge_free_blocks(block);
  }
}

struct Free_Block_Search_Result {
  Heap::Block *previous_free_block = nullptr;
  Heap::Block *free_block          = nullptr;
};

static Free_Block_Search_Result find_free_block_that_fit (Heap::Block * const free_list, const usize size) {
  Free_Block_Search_Result result;

  for (auto it = free_list; it; it = it->next_free) {
    assert(it->is_free());

    if (size <= it->size) {
      result.free_block = it;
      break;
    }

    result.previous_free_block = it;
  }

  return result;
}

u8* Heap::alloc (const usize size, const char *tag) {
  assert(size != 0);
  assert(tag != nullptr);

  assert(this->free_blocks != nullptr);

  if (this->free_blocks == nullptr) return nullptr;

  const usize aligned_size = align_forward(size, default_alignment);

  // Check for overflow
  if (aligned_size < size) return nullptr; 

  auto [prev_free, block] = find_free_block_that_fit(this->free_blocks, aligned_size);

  // No block that's capable of holding the requested size has been found
  if (block == nullptr) return nullptr;

  assert(block->is_free());

  const usize size_of_trailing_space_after_allocation = block->size - aligned_size;
  if (size_of_trailing_space_after_allocation < Block_Min_Size) {

    /*
      If the trailing space that's left after allocating the requested size in the current block
      is not enough to cut a new block, then we just leave it as is and remove this block from the free
      blocks list.
    */
    if (prev_free != nullptr) prev_free->next_free = block->next_free;
    else                      this->free_blocks    = block->next_free;
  }
  else {
    /*
      There's enough trailing space to cut a new block.
    */
    auto *new_block = partition_block(block, aligned_size);

    assert(is_aligned_by(new_block->memory(), default_alignment));
    assert(new_block->size == (size_of_trailing_space_after_allocation - sizeof(Block)));

    if (prev_free != nullptr) prev_free->next_free = new_block;
    else                      this->free_blocks    = new_block;
  }

  block->header = Block::Header(Block::In_Use, tag);

  return block->memory();
}

u8* Heap::realloc (void * const ptr, const usize new_size, const char * const tag) {
  assert(new_size > 0);
  assert(tag != nullptr);

  if (ptr == nullptr) return alloc(new_size, tag);

  const usize aligned_size = align_forward(new_size, default_alignment);
  // check the overflow
  if (aligned_size < new_size) return nullptr; 

  Block &block = *get_memory_block(ptr);
  assert(block.is_used());

  if (block.size == aligned_size) return reinterpret_cast<u8*>(ptr);

  // Get the next block-in-chain if it's currently empty
  Block *next_block = (block.next_block && block.next_block->is_free()) ? block.next_block : nullptr;
  
  if (aligned_size < block.size) { // Request to shrink the size of the current block.
    block.header.tag = reinterpret_cast<usize>(tag);
    
    if (next_block) { // Handle shrinking by backward extending the next-in-chain free block
      auto *previous_free_block = get_previous_free_block(this->free_blocks, next_block);

      /*
        Move the header of the next block that we are extending to the new start position of the region
        TODO: Should I check for overlapping here?
      */
      auto *new_block_start_position    = block.memory() + aligned_size;
      const usize next_block_extension_size = reinterpret_cast<u8*>(next_block) - new_block_start_position;

      // Move next block info to a new position
      const usize next_block_extended_size = next_block->size + sizeof(Block) + next_block_extension_size;

      memmove_s(new_block_start_position, next_block_extended_size, next_block, sizeof(Block));

      next_block = reinterpret_cast<Block*>(new_block_start_position);
      next_block->size += next_block_extension_size;

      block.size       = aligned_size;
      block.next_block = next_block;

      if (previous_free_block != nullptr) previous_free_block->next_free = next_block;
      else                                this->free_blocks = next_block;
    }
    else { // Next block for some reason is not available for backward extension
      const usize potential_free_space = block.size - aligned_size;
      if (potential_free_space >= Block_Min_Size) {
        auto new_block = partition_block(&block, aligned_size);
        assert(new_block->size == (potential_free_space - sizeof(Block)));
        push_block_to_the_free_list(&this->free_blocks, new_block);
      }
    }

    assert(get_memory_block(ptr)->is_used());
    return reinterpret_cast<u8*>(ptr);
  }

  if (const usize grow_space = aligned_size - block.size; next_block && (next_block->size >= grow_space)) {
    // Grow the current allocation, by consuming either the required part of the next block or the entire next block
    Block * const previous_free = get_previous_free_block(this->free_blocks, next_block);

    if (auto free_space = next_block->size - grow_space; 
        free_space >= Block_Min_Size) { // Free space left in the current block is enough for a new block to be allocated      
                                                                                                  // Move the next block's header after the grow space boundary
                                                                                                  u8 * const new_position = reinterpret_cast<u8 *>(next_block) + grow_space;  
      memmove_s(new_position, free_space, next_block, sizeof(Block));
      next_block = reinterpret_cast<Block*>(new_position);
      next_block->size -= grow_space;

      block.size       += grow_space;
      block.next_block  = next_block;
      
      if (previous_free != nullptr) {
        previous_free->next_free = next_block;
      }
      else {
        this->free_blocks = next_block;
      }
    }
    else { // Consume next block entirely into the current block
      block.size       += next_block->size + sizeof(Block);
      block.next_block  = next_block->next_block;

      if (previous_free != nullptr) {
        previous_free->next_free = next_block->next_free;
      }
      else {
        this->free_blocks = next_block->next_free;
      }
    }

    assert(get_memory_block(ptr)->is_used());
    return reinterpret_cast<u8*>(ptr);
  }

  // Resize attempt has failed, get a new block and copy stuff there.
  auto new_ptr = this->alloc(aligned_size, tag);
  if (new_ptr != nullptr) {
    assert(get_memory_block(new_ptr)->is_used());
    memcpy_s(new_ptr, aligned_size, ptr, block.size);
    this->free(ptr, tag);
  }

  return new_ptr;
}

void Heap::free (void * const ptr, const char * const tag) {
  assert(ptr != nullptr);
  assert(tag != nullptr);
  
  auto block = get_memory_block(ptr);
  assert(block->is_used());

  block->header = Block::Header(Block::Free, tag);

  push_block_to_the_free_list(&this->free_blocks, block);
}

void Heap::report_leaks () const {
  if constexpr (DEBUG_HEAP_ENABLE_REPOTING) {
    for (const Block *block = this->blocks; block != nullptr; block = block->next_block) {
      const char *tag = block->read_tag();
      assert_fmsg(block->is_free(), "Memory leak found: %s", tag);
    }
  }
}

const char* Heap::find_tag (void *ptr) {
  const auto block = get_memory_block(ptr);
  return reinterpret_cast<const char*>(block->header.tag);
}

}
