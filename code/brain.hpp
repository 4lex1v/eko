
#pragma once

#include "anyfin/list.hpp"
#include "anyfin/arena.hpp"
#include "anyfin/result.hpp"

#include "eko.hpp"
#include "ast.hpp"

struct Binding;
struct Type_Binding;

enum struct Built_In_Type: u8 {
  Void,
  Bool,
  Signed_Byte,
  Unsigned_Byte,
  Signed_Half_Word,
  Unsigned_Half_Word,
  Signed_Word,
  Unsigned_Word,
  Signed_Double_Word,
  Unsigned_Double_Word,

  Float,
  Double,
  
  String_Literal,
};

enum struct Type_Kind: u8 {
  Built_In,
  Struct,
  Pointer,
  Array,
  Seq
};

struct Type {
  using enum Type_Kind;

  Type_Kind kind;
  Built_In_Type built_in_type;
  Type *element = nullptr;
  const Type_Binding *binding = nullptr;
};

enum struct Value_Kind: u8 {
  Immediate,
  Memory
};

struct Immediate_Value {
  usize value;
  Type  type;
};

struct Memory_Value {
  
};

struct Value {
  using enum Value_Kind;

  Value_Kind kind;
  union {
    Immediate_Value immediate;
    Memory_Value    memory;
  };
};

struct Entry;

enum struct Entry_Kind {
  Load,
  Store,
  Call,
  Return,
};

#define KIND_TAG(TYPE, KIND) static const auto kind = TYPE::KIND

#define ENTRY_KIND(KIND) KIND_TAG(Entry_Kind, KIND)

struct Return_Entry {
  ENTRY_KIND(Return);

  Fin::List<Entry> expr;
};

struct Entry {
  using enum Entry_Kind;

  Entry_Kind kind;
  union {
    Return_Entry return_entry;
  };

  template <typename T>
  Entry (T value):
    kind { T::kind }
  {
    new (&this->return_entry) T(Fin::move(value));
  }
};

enum struct Binding_Kind {
  Value,
  Type,
  Lambda
};

#define BINDING_KIND(KIND) \
    static const auto kind = Binding_Kind::KIND;

struct Value_Binding {
  BINDING_KIND(Value);

  struct Variable_Node *node;

  Type *type;
  Type *init_expr_type;

  bool is_constant;
};

struct Type_Binding {
  BINDING_KIND(Type);

  const Struct_Node *node;
  Scope scope;
};

struct Lambda_Binding {
  BINDING_KIND(Lambda);

  const Lambda_Node *node;
  Scope scope;

  Fin::List<Value_Binding> params;
  Type return_type;

  Fin::List<Entry> entries;
};

struct Binding {
  using enum Binding_Kind;

  Binding_Kind kind;
  union {
    Value_Binding  value_binding;
    Type_Binding   type_binding;
    Lambda_Binding lambda_binding;
  };

  template <typename T>
  Binding (T value):
    kind { T::kind }
  {
    new (&this->value_binding) T(Fin::move(value));
  }
};

struct Typer_Error {
  enum Kind {
    Error
  };

  Kind kind;
};

template <typename T> using Result = Fin::Result<Typer_Error, T>;

Result<void> typecheck (Fin::Memory_Arena &arena, Source_File &file);
