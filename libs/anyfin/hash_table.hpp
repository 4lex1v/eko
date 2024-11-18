
#pragma once

#include "prelude.hpp"
#include "allocator.hpp"

// TODO: #ux Can I wrap these hashes into a struct?

namespace Fin {

constexpr static inline usize seed = 0x517cc1b727220a95;

static inline u64 compute_hash (const u64 value) {
  const u8 *bytes = reinterpret_cast<const u8 *>(&value);

  size_t hash = bytes[0] | (bytes[1] << 8) | (bytes[2] << 16) | (bytes[3] << 24);
  hash |= (size_t) (bytes[4] | (bytes[5] << 8) | (bytes[6] << 16) | (bytes[7] << 24)) << 16 << 16;
  hash ^= seed;
  hash = (~hash) + (hash << 21);
  hash ^= rotate_right(hash,24);
  hash *= 265;
  hash ^= rotate_right(hash,14);
  hash ^= seed;
  hash *= 21;
  hash ^= rotate_right(hash,28);
  hash += (hash << 31);
  hash = (~hash) + (hash << 18);

  return hash;
}

static inline u64 h1_hash (const u64 key_hash) {
  return (key_hash >> 7);
}

static inline u8 h2_hash (const u64 key_hash) {
  return (key_hash & 0x7F);
}

template <typename Key, typename Value>
struct Hash_Table {
  struct Slot { Key key; Value value; };

  enum struct Control_Byte: s8 {
    Empty   = -128,
    Removed = -1,
  };

  using CB = Control_Byte;
  
  constexpr static f32 Growth_Ratio = 0.875f;

  Control_Byte default_controls [16] {
    CB::Empty, CB::Empty, CB::Empty, CB::Empty, CB::Empty, CB::Empty, CB::Empty, CB::Empty,
    CB::Empty, CB::Empty, CB::Empty, CB::Empty, CB::Empty, CB::Empty, CB::Empty, CB::Empty
  };

  Allocator allocator;
  void *memory = nullptr;

  Control_Byte *controls      = reinterpret_cast<Control_Byte *>(default_controls);
  Slot         *slots         = nullptr;
  usize         capacity      = 0;
  usize         until_growth  = static_cast<usize>(Growth_Ratio * (f32) capacity);

  usize count = 0;

  Hash_Table () = default;
  Hash_Table (Allocator _allocator):
    allocator { _allocator }
  {}
  
  decltype(auto) find (this auto &&self, const Key &key) {
    auto slot = self.find_slot(key);
    return slot ? &slot->value : nullptr;
  }

  bool contains (this auto &&self, const Key& key) {
    return self.find_slot(key) != nullptr;
  }

  void insert (Key &&key, Value &&value) {
    if (auto slot = find_slot(key); slot) {
      slot->value = move(value);  
      return;
    }

    const auto key_hash = compute_hash(key);

    auto target = find_position_for_insertion(key_hash);
    if (until_growth == 0 && controls[target.offset] != Control_Byte::Removed) {
      grow_if_needed();
      target = find_position_for_insertion(key_hash);
    }

    auto slot_offset = target.offset;

    assert_msg(slot_offset >= 0, "Something went wrong while looking up the slot to store the value");

    auto &slot = slots[slot_offset];
    slot.key   = move(key);
    slot.value = move(value);

    set_control_byte(slot_offset, h2_hash(key_hash));

    this->count        += 1;
    this->until_growth -= 1;
  }

  void insert_copy (const Key &key, Value value) { insert(Key(key), move(value)); }

  void remove (const Key &key) {
    const auto key_hash = compute_hash(key);
    auto probe          = Probe_Position(key_hash, this->capacity);

    u32 match_array[16] = {0};
    while (true) {
      auto group = Group(this->controls + probe.position);

      const auto matches_count = group.match(h2_hash(key_hash), match_array);
      for (u8 index = 0; index < matches_count; index++) {
        const auto slot = slots + probe.offset(match_array[index]);
        if (key != slot->key) continue;

        const usize slot_offset = probe.offset(match_array[index]);
        set_control_byte(slot_offset, cast_enum(CB::Removed));

        this->count        -= 1;
        this->until_growth += 1;
      }

      if (group.is_empty()) return;

      probe.next();

      fin_ensure(probe.index <= this->capacity);
    }
  }
  
  void resize (const usize new_capacity) {
    auto *old_controls = controls;
    auto *old_slots    = slots;
    auto  old_capacity = capacity;

    allocate_backing_memory(new_capacity);

    usize total_probe_length = 0;
    for (usize index = 0; index < old_capacity; index++) {
      if (cast_enum(old_controls[index]) < 0) continue; // if MSB is not set, it's a filled slot

      const auto hash            = compute_hash(old_slots[index].key);
      const auto insert_position = find_position_for_insertion(hash);

      total_probe_length += insert_position.probe_length;

      set_control_byte(insert_position.offset, h2_hash(hash));
      runtime_move_memory(slots + insert_position.offset, old_slots + index);
    }

    if (old_capacity) heap->free(old_controls, tag);
  }
  
  void destroy () {
    if (this->controls == default_controls) return;

    free(this->allocator, this->memory);
  }

private:
  decltype(auto) find_slot (this auto &&self, const Key &key) {
    const auto key_hash = compute_hash(key);

    auto probe = Probe_Position(key_hash, self.capacity);

    u32 match_array[16] = {0};
    while (true) {
      auto group = Group(self.controls + probe.position);

      const auto element_hash     = h2_hash(key_hash);
      const auto elements_matched = group.match(element_hash, match_array);
      for (u8 index = 0; index < elements_matched; index++) {
        const auto slot = self.slots + probe.offset(match_array[index]);
        if (key == slot->key) return slot;
      }

      if (group.is_empty()) return nullptr;

      probe.next();

      assert_msg(probe.index <= self.capacity, "Table is full");
    }
  }

  void allocate_backing_memory (const usize new_capacity) {
    assert_msg(new_capacity > 0, "Attempt to initialize the hash table with 0 capacity");

    this->capacity     = new_capacity;
    this->until_growth = static_cast<usize>(Growth_Ratio * (f32) new_capacity) - this->count;

    /*
      Allocates two arrays of "capacity" size, one to hold the control bytes (u8) and the other one
      to store the actual data (Slots). Additional 15 bytes are added to the end of the control array
      so that we could efficiently scan the control bytes from any position, these bytes reflect the
      beginning of the controls array.
     */
    const usize controls_size   = capacity + 15;
    const usize allocation_size = controls_size + (capacity * sizeof(Slot));

    u8 *memory = heap->alloc(allocation_size, tag);

    controls = reinterpret_cast<Control_Byte *>(memory);
    slots    = reinterpret_cast<Slot *>(memory + controls_size);

    runtime_fill_memory(reinterpret_cast<u8 *>(controls), controls_size, cast_enum(Control_Byte::Empty));
  }

  void set_control_byte (const usize offset, const u8 control_byte_value) {
    this->controls[offset] = static_cast<Control_Byte>(control_byte_value);
    this->controls[((offset - 15) & (this->capacity - 1)) + 15] = static_cast<Control_Byte>(control_byte_value); 
  }

  /*
  TODO(#efficiency):
    Google's implementation also offers an improvement to cleanup deleted slots and rehash in place without growing.
    Worth to check this out at some point, if it would be necessary for my purposes.
 */
  void grow_if_needed () {
    if (capacity == 0) [[unlikely]] resize(16);
    else                            resize(capacity << 1);
  }

  struct Probe_Position {
    usize mask, position, index;

    Probe_Position (const u64 key_hash, const usize capacity):
      mask     { capacity ? capacity - 1 : 0 },
      position { h1_hash(key_hash) & mask },
      index    { 0 }
    {
      fin_ensure(is_power_of_2(capacity));
    }

    void  next   ()                    { index += 16; position += index; position &= mask; }
    usize offset (const usize i) const { return (position + i) & mask; }
  };

  struct Group {
    __m128i control;

    explicit Group (const Control_Byte *controls):
      control { _mm_loadu_si128(reinterpret_cast<const __m128i*>(controls)) }
    {}

    /*
      Find if the control group contains elements with a given H2 part of the key's hash.
      Since the H2 hash is only 7 bits, there's a chance that multiple elements within the group may
      have the same value. To handle multiple possible value, we iterate the array and save indicies
      of these elements.
     */
    u32 match (const u8 hash, u32 (&match_array)[16]) const {
      const u32 matches = static_cast<u32>(_mm_movemask_epi8(_mm_cmpeq_epi8(_mm_set1_epi8(hash), this->control)));
      if (!matches) return 0;

      u32 matches_count = 0;
      for (u8 index = 0, offset = 0; index < 16; index++) {
        if ((matches >> index) & 0x1) {
          match_array[offset++]  = index;
          matches_count         += 1;
        }
      }

      assert_msg(matches_count > 0, "Something went wrong, the number of matches should not be 0");

      return matches_count;
    }

    u16 empty_or_deleted_slots_mask () const {
      return static_cast<u16>(_mm_movemask_epi8(_mm_cmplt_epi8(this->control, _mm_set1_epi8(0))));
    }

    bool is_empty () const {
      return static_cast<bool>(_mm_movemask_epi8(_mm_cmpeq_epi8(_mm_set1_epi8(cast_enum(Control_Byte::Empty)), this->control)));
    }
  };

  struct Insert_Position {
    usize offset;
    usize probe_length;

    Insert_Position (const usize position, const Probe_Position &probe):
      offset       { probe.offset(position) },
      probe_length { probe.index }
    {}
  };

  Insert_Position find_position_for_insertion (const u64 key_hash) const {
    auto probe = Probe_Position(key_hash, this->capacity);

    while (true) {
      auto group = Group(this->controls + probe.position);

      const auto mask = group.empty_or_deleted_slots_mask();
      if (mask) {
        const auto position = count_trailing_zeros(mask);
        return Insert_Position(position, probe);
      }

      probe.next();

      fin_ensure(probe.index <= this->capacity);
    }
  }

};

}
