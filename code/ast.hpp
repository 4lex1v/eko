
#pragma once

#include "anyfin/array.hpp"
#include "anyfin/list.hpp"

#include "tokens.hpp"

using namespace Fin;

struct Node {
  enum Node_Kind {
    Undefined,
    Root,

    Decl_Value,
    Decl_Lambda,
    Decl_Struct,

    Type,
    Parameter,

    Identifier,
    Literal,

    Function_Call,
    Return
  };

  Node_Kind kind = Undefined;
};

struct Root_Node: Node {
  List<Node *> nodes;
};

struct Type_Node: Node {
  enum Type_Node_Kind { Pointer, Array, Seq, Plain };

  Type_Node_Kind kind;

  Type_Node (Type_Node_Kind _kind):
    Node(Type),
    kind { _kind }
  {}
};

struct Plain_Type_Node: Type_Node {
  Token type_name;
  Fin::Array<Type_Node *> parameters;

  Plain_Type_Node (Token name, Fin::Array<Type_Node *> params = {}):
    Type_Node(Plain),
    type_name  { Fin::move(name) },
    parameters { params }
  {}
};

struct Pointer_Type_Node: Type_Node {
  Type_Node *value_type;

  Pointer_Type_Node (Type_Node *_value_type = nullptr):
    Type_Node(Pointer),
    value_type { _value_type }
  {}
};

struct Array_Type_Node: Type_Node {
  Node      *bounds_expression;
  Type_Node *elements_type;

  Array_Type_Node (Node *expr = nullptr, Type_Node *_elements_type = nullptr):
    Type_Node(Array),
    bounds_expression { expr },
    elements_type     { _elements_type }
  {}
};

struct Seq_Type_Node: Type_Node {
  Type_Node *element_type;

  Seq_Type_Node (Type_Node *_element_type = nullptr):
    Type_Node(Seq),
    element_type { _element_type }
  {}
};

struct Parameter_Node: Node {
  Token name;
  Type_Node *type         = nullptr;
  Node      *default_init = nullptr;

  Parameter_Node ():
    Node(Parameter)
  {}
};

struct Literal_Node: Node {
  Token value;

  Literal_Node (Token _value):
    Node(Literal),
    value { move(_value) }
  {}
};

struct Identifier_Node: Node {
  /*
    Identifier expression could be however long, consisting of multiple parts
   */
  List<Token> identifier;

  Identifier_Node (List<Token> ids = {}):
    Node(Identifier),
    identifier { move(ids) }
  {}
};

struct Value_Decl_Node: Node {
  Token name;
  Node *expr;

  Value_Decl_Node (Token _name, Node *_expr = nullptr, bool is_const = false):
    Node(Decl_Value),
    name { move(_name) },
    expr { _expr }
  {}
};

struct Struct_Decl_Node: Node {
  Token name;

  List<Parameter_Node> params;
  List<Parameter_Node> fields;

  Struct_Decl_Node (Token _name):
    Node(Decl_Struct),
    name { move(_name) }
  {}
};

struct Lambda_Decl_Node: Node {
  Token name;

  List<Parameter_Node> params;
  Type_Node *return_type;
  List<Node *> body;

  Lambda_Decl_Node (Token _name):
    Node(Decl_Lambda),
    name { move(_name) }
  {}
};

struct Function_Call_Node: Node {
  Node *expr = nullptr;
  List<Node *> args = {};

  Function_Call_Node (): Node(Function_Call) {}
};

struct Return_Node: Node {
  Node *expr = nullptr;

 Return_Node (Node *_expr): Node(Return), expr{_expr} {}
};
