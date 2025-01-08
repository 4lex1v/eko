
#pragma once

#include "anyfin/list.hpp"
#include "anyfin/arena.hpp"
#include "anyfin/result.hpp"
#include "anyfin/bit_mask.hpp"

#include "eko.hpp"
#include "ast.hpp"
#include "utils.hpp"

struct Binding;
struct Type_Binding;

enum struct Built_In_Type: u8 {
  Void,
  Integer,
  Floating,
  String_Literal,
};

enum struct Type_Kind: u8 {
  Built_In,
  Struct,
  Pointer,
  Array,
  Seq
};

enum struct Type_Flags {
  // Numeric Flags
  Unsigned    = fin_flag(0),
  Bit         = fin_flag(1),
  Byte        = fin_flag(2),
  Half_Word   = fin_flag(3),
  Word        = fin_flag(4),
  Double_Word = fin_flag(5),

  // Floating Flags
  Double      = fin_flag(0),
};

struct Type {
  using enum Type_Kind;

  Type_Kind kind;
  Built_In_Type built_in_type;

  Fin::Bit_Mask<Type_Flags> flags;

  const Type         *element = nullptr;
  const Type_Binding *binding = nullptr;

  GEN_KIND_CHECK(kind);
  GEN_KIND_CHECK(built_in_type);
};

enum struct Value_Kind: u8 {
  Immediate,
  Memory
};

#define VALUE_KIND(KIND) KIND_TAG(Value_Kind, KIND)

struct Immediate_Value {
  VALUE_KIND(Immediate);

  enum Kind: u8 {
    String,
    Integer,
    Float,
    Double
  };

  Kind  imm_kind;
  usize value;

  const Type *type;
};

struct Load_Entry;
struct Access_Entry;

enum struct Memory_Value_Kind: u8 {
  Access,
  Load
};

/*
  Could be a result of a function call, if it returns a value, or a be a load operation
 */
struct Memory_Value {
  using enum Memory_Value_Kind;
  
  VALUE_KIND(Memory);

  Memory_Value_Kind mem_kind;
  union {
    const Access_Entry *access_entry;
    const Load_Entry   *load_entry;
  };
};

struct Value {
  using enum Value_Kind;

  Value_Kind kind;
  union {
    Immediate_Value immediate;
    Memory_Value    memory;
  };

  GEN_CONSTRUCTOR(Value, kind, immediate);
};

struct Entry;

enum struct Entry_Kind {
  Access,
  Load,
  Store,
  Call,
  Return,
};

#define ENTRY_KIND(KIND) KIND_TAG(Entry_Kind, KIND)

struct Load_Entry {
  ENTRY_KIND(Load);
};

struct Return_Entry {
  ENTRY_KIND(Return);

  Value value;
};

struct Entry {
  using enum Entry_Kind;

  Entry_Kind kind;
  union {
    Return_Entry return_entry;
    Load_Entry   load_entry;
  };

  GEN_CONSTRUCTOR(Entry, kind, return_entry);
};

enum struct Binding_Kind {
  Value,
  Type,
  Lambda,
  Ambiguous,
};

#define BINDING_KIND(KIND) KIND_TAG(Binding_Kind, KIND);

struct Value_Binding {
  BINDING_KIND(Value);

  const Type *type;

  bool is_constant;
};

/*
  Type bindings represent both struct declarations as well as regular type binding.
 */
struct Type_Binding {
  BINDING_KIND(Type);

  enum Type_Binding_Kind: u8 { Built_In, Struct, Alias };

  Type_Binding_Kind binding_kind;

  const Struct_Node *node;
  Scope scope = {};

  /*
    TODO: @arch
    Should the type be stored somewhere else?
   */
  union {
    Type               *type_value;
    const Type_Binding *alias;
  };

  GEN_KIND_CHECK(binding_kind);
};

struct Lambda_Binding {
  BINDING_KIND(Lambda);

  const Lambda_Node *node;
  Scope scope;

  Fin::List<Value_Binding> params;
  const Type *return_type;

  Fin::List<Entry> entries;
};

struct Ambiguous_Binding {
  BINDING_KIND(Ambiguous);

  const Constant_Node *node;
};

struct Binding {
  using enum Binding_Kind;

  Binding_Kind kind;
  union {
    Value_Binding      value_binding;
    Type_Binding       type_binding;
    Lambda_Binding     lambda_binding;
    Ambiguous_Binding  ambiguous_binding;
  };

  GEN_CONSTRUCTOR(Binding, kind, value_binding);
  GEN_KIND_CHECK(kind);
};

using Type_Cache = Fin::Hash_Table<Fin::String, const Binding *>;

struct Typer_Error {
  enum Kind {
    Error
  };

  Kind kind;
};

template <typename T> using Result = Fin::Result<Typer_Error, T>;

void init_typer ();

Result<void> typecheck (Source_File &file);
