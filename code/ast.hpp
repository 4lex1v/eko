
#pragma once

#include "anyfin/array.hpp"
#include "anyfin/list.hpp"
#include "anyfin/meta.hpp"

#include "tokens.hpp"

using Fin::Array;
using Fin::List;
using Fin::move;

namespace Eko {

struct Node;
struct Type_Node;

struct Root_Node {
  List<Node *> nodes;
};

enum struct Node_Kind {
  Undefined,

  Identifier,
  Literal,
  Type,

  Value_Decl,
  Var_Decl,
  Lambda_Decl,
  Struct_Decl,

  Parameter,
  Member_Access,
  Function_Call,
  Return
};

enum struct Type_Node_Kind {
   Pointer, Array, Seq, Plain 
};

struct Plain_Type_Node {
  static const auto kind = Type_Node_Kind::Plain;

  Token type_name;
  List<Type_Node *> parameters;
};

struct Pointer_Type_Node {
  static const auto kind = Type_Node_Kind::Pointer;
  
  Type_Node *value_type;
};

struct Array_Type_Node {
  static const auto kind = Type_Node_Kind::Array;

  Node      *bounds_expression;
  Type_Node *elements_type;
};

struct Seq_Type_Node {
  static const auto kind = Type_Node_Kind::Seq;

  Type_Node *element_type;
};

struct Type_Node {
  static const auto kind = Node_Kind::Type;

  Type_Node_Kind type_kind;

  union {
    Plain_Type_Node   plain_type;
    Pointer_Type_Node pointer_type;
    Array_Type_Node   array_type;
    Seq_Type_Node     seq_type;
  };

  template <typename T>
  Type_Node (T value): type_kind { T::kind } {
    new (&plain_type) T(move(value));
  }
};

struct Identifier_Node {
  static const auto kind = Node_Kind::Identifier;

  Token identifier;
};

struct Member_Access_Node {
  static const auto kind = Node_Kind::Member_Access;

  Node *expr;
  Token member;
};

struct Literal_Node {
  static const auto kind = Node_Kind::Literal;

  Token value;
};

struct Parameter_Node {
  static const auto kind = Node_Kind::Parameter;

  Token      name;
  Type_Node *type;
  Node      *init_expr;
};

struct Value_Decl_Node {
  static const auto kind = Node_Kind::Value_Decl;

  Token  name;
  Node  *expr;
};

struct Var_Decl_Node {
  static const auto kind = Node_Kind::Var_Decl;

  Token  name;
  Node  *expr;
};

struct Struct_Decl_Node {
  static const auto kind = Node_Kind::Struct_Decl;
  
  Token name;

  List<Parameter_Node> params;
  List<Parameter_Node> fields;
};

struct Lambda_Decl_Node {
  static const auto kind = Node_Kind::Lambda_Decl;

  Token name;

  List<Parameter_Node>  params;
  Type_Node            *return_type;
  List<Node *>          body;
};

struct Function_Call_Node {
  static const auto kind = Node_Kind::Function_Call;

  Node *expr;
  List<Node *> args;
};

struct Return_Node {
  static const auto kind = Node_Kind::Return;

  Node *expr;
};

struct Node {
  using enum Node_Kind;

  Node_Kind kind;

  union {
    Identifier_Node identifier;
    Literal_Node    literal;
    Type_Node       type;

    Value_Decl_Node  value_decl;
    Var_Decl_Node    var_decl;
    Lambda_Decl_Node lambda_decl;
    Struct_Decl_Node struct_decl;

    Parameter_Node     parameter;
    Member_Access_Node member;
    Function_Call_Node func_call;
    Return_Node        return_expr;
  };

  template <typename T>
  Node (T value): kind { T::kind } {
    new (&identifier) T(move(value));
  }
    
};

}
