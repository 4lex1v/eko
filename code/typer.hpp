
#pragma once

#include "anyfin/list.hpp"

#include "eko.hpp"
#include "ast.hpp"

struct Type {
  
};

struct Binding;

enum struct Binding_Kind {
  Value,
  Struct,
  Lambda
};

struct Value_Binding {
  struct Variable_Node *node;

  Type type;
  Type init_expr_type;

  bool is_constant;
};

struct Struct_Binding {
  const Struct_Node *node;
  Scope scope;
  
  Fin::List<Binding> fields;
};

struct Lambda_Binding {
  const Lambda_Node *node;
  Scope scope;

  Fin::List<Value_Binding> params;
  Type return_type;
};

struct Binding {
  using enum Binding_Kind;

  Binding_Kind kind;
  union {
    Value_Binding variable_binding;
    Struct_Binding   struct_binding;
    Lambda_Binding   lambda_binding;
  };
};

void typecheck (Source_File &file);
