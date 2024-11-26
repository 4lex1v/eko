
#pragma once

#include "anyfin/list.hpp"
#include "anyfin/arena.hpp"
#include "anyfin/option.hpp"

#include "eko.hpp"
#include "ast.hpp"

struct Binding;
struct Type_Binding;

enum struct Type_Kind: u8 {
  Basic_Void,
  Basic_Bool,
  Basic_Signed_Byte,
  Basic_Unsigned_Byte,
  Basic_Signed_Half_Word,
  Basic_Unsigned_Half_Word,
  Basic_Signed_Word,
  Basic_Unsigned_Word,
  Basic_Signed_Double_Word,
  Basic_Unsigned_Double_Word,
  Basic_Float,
  Basic_Double,
  Basic_String_Literal,

  Struct,
  Pointer,
  Array,
  Seq
};

struct Type {
  using enum Type_Kind;

  Type_Kind kind;
  Type *element = nullptr;
  const Type_Binding *binding = nullptr;
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

Fin::Option<Typer_Error> typecheck (Fin::Memory_Arena &arena, Source_File &file);
