
#pragma once

#include "anyfin/array.hpp"
#include "anyfin/list.hpp"
#include "anyfin/meta.hpp"
#include "anyfin/bit_mask.hpp"

#include "eko.hpp"
#include "tokens.hpp"
#include "utils.hpp"

struct Type;
struct Struct_Binding;
struct Lambda_Binding;

struct Node;
struct Type_Node;

enum struct Node_Kind: u8 {
  Expression,
  Declaration,
  Statement
};

#define NODE_KIND(KIND) \
  static const auto kind = Node_Kind::KIND;

struct Expression_Node;

enum struct Expression_Node_Kind: u8 {
  Identifier,
  Member_Access,
  Literal,
  Function_Call,
  Binary_Expr,
  Star_Expr,
  As_Cast,
  Post_If_Expr,
};

#define EXPR_KIND(KIND) \
  static const auto kind = Expression_Node_Kind::KIND

enum struct Literal_Node_Kind: u8 {
  String,
  Signed_Integer,
  Unsigned_Integer,
  Float,
  Double,
  Null
};

struct Literal_Node {
  using enum Literal_Node_Kind;

  EXPR_KIND(Literal);

  Literal_Node_Kind lit_kind;
  union {
    Fin::String string_value;
    s64         sint_value;
    u64         uint_value;
    float       float_value;
    double      double_value;
  };

  bool is_signed = false;

  GEN_KIND_CHECK(lit_kind);
};

struct Identifier_Node {
  EXPR_KIND(Identifier);

  Token value;
};

struct Member_Access_Node {
  EXPR_KIND(Member_Access);

  Expression_Node *expr;
  Token member;
};

struct Function_Call_Node {
  EXPR_KIND(Function_Call);

  Expression_Node *expr;
  Fin::List<Expression_Node> args;
};

struct Binary_Expr_Node {
  EXPR_KIND(Binary_Expr);

  Expression_Node *left;
  Expression_Node *right;
};

struct Star_Expr_Node {
  EXPR_KIND(Star_Expr);

  Expression_Node *expr;
};

struct As_Cast_Expr_Node {
  EXPR_KIND(As_Cast);

  Expression_Node *expr;
  Type_Node       *type_node;
};

struct Post_If_Expr_Node {
  EXPR_KIND(Post_If_Expr);

  Expression_Node *condition;
  Expression_Node *position_branch;
  Expression_Node *negative_branch;
};

struct Expression_Node {
  using enum Expression_Node_Kind;

  NODE_KIND(Expression);
  
  Expression_Node_Kind expr_kind;
  union {
    Literal_Node       literal_expr;
    Identifier_Node    identifier_expr;
    Member_Access_Node member_access_expr;
    Function_Call_Node function_call_expr;
    Binary_Expr_Node   binary_expr;
    Star_Expr_Node     star_expr;
    As_Cast_Expr_Node  as_cast_expr;
    Post_If_Expr_Node  post_if_expr;
  };

  GEN_CONSTRUCTOR(Expression_Node, expr_kind, literal_expr);
  GEN_KIND_CHECK(expr_kind);
};

struct Type_Node;

enum struct Type_Node_Kind: u8 {
  Pointer,
  Array,
  Seq,
  Plain 
};

#define TYPE_KIND(KIND) \
  static const auto kind = Type_Node_Kind::KIND 

struct Plain_Type_Node {
  TYPE_KIND(Plain);

  Token name;
  Fin::List<Type_Node> parameters;
};

struct Pointer_Type_Node {
  TYPE_KIND(Pointer);
  
  Type_Node *value_type;
};

struct Array_Type_Node {
  TYPE_KIND(Array);

  Type_Node       *elements_type;
  Expression_Node  bounds;
};

struct Seq_Type_Node {
  TYPE_KIND(Seq);

  Type_Node *element_type;
};

struct Type_Node {
  using enum Type_Node_Kind;

  Type_Node_Kind type_kind;
  union {
    Plain_Type_Node   plain_type;
    Pointer_Type_Node pointer_type;
    Array_Type_Node   array_type;
    Seq_Type_Node     seq_type;
  };

  GEN_CONSTRUCTOR(Type_Node, type_kind, plain_type);
  GEN_KIND_CHECK(type_kind);
};

struct Declaration_Node;

enum struct Declaration_Node_Kind: u8 {
  Constant,
  Variable,
  Lambda,
  Struct
};

#define DECL_KIND(KIND) \
  static const auto kind = Declaration_Node_Kind::KIND;

struct Constant_Node {
  DECL_KIND(Constant);

  Token name;

  Expression_Node expr;
};

struct Variable_Node {
  DECL_KIND(Variable);

  Token       name;

  Type_Node       *type = nullptr;
  Expression_Node *expr = nullptr;
};

struct Struct_Node {
  DECL_KIND(Struct);
  
  Token name;

  Fin::List<Variable_Node>    params;
  Fin::List<Declaration_Node> fields;

  Struct_Binding *binding = nullptr;
};

enum struct Lambda_Node_Flags {
  Extern_Decl = fin_flag(0),
};

struct Lambda_Node {
  DECL_KIND(Lambda);

  Token name;

  Fin::List<Variable_Node>  params;
  Fin::List<Node>           body;

  Type_Node *return_type = nullptr;

  Fin::Bit_Mask<Lambda_Node_Flags> flags {};
};

struct Declaration_Node {
  using enum Declaration_Node_Kind;
  
  NODE_KIND(Declaration);

  Declaration_Node_Kind decl_kind;
  union {
    Constant_Node constant_decl;
    Variable_Node variable_decl;
    Struct_Node   struct_decl;
    Lambda_Node   lambda_decl;
  };

  Fin::String name; 
    
  template <typename T>
  Declaration_Node (T value):
    decl_kind { T::kind }
  {
    auto _value = new (&this->constant_decl) T(Fin::move(value));
    name = _value->name.value;
  }

};

struct Statement_Node;

enum struct Statement_Node_Kind: u8 {
  Return
};

#define STMNT_KIND(KIND) \
  static const auto kind = Statement_Node_Kind::KIND

struct Return_Node {
  STMNT_KIND(Return);

  Expression_Node value;
};

struct Statement_Node {
  using enum Statement_Node_Kind;

  NODE_KIND(Statement);

  Statement_Node_Kind stmnt_kind;
  union {
    Return_Node return_stmnt;
  };

  template <typename T>
  Statement_Node (T value):
    stmnt_kind { T::kind }
  {
    new (&this->return_stmnt) T(Fin::move(value));
  }
};

struct Node {
  using enum Node_Kind;

  Node_Kind kind;

  union {
    Expression_Node  expr_node;
    Declaration_Node decl_node;
    Statement_Node   stmnt_node;
  };

  template <typename T>
  Node (T value):
    kind { T::kind }
  {
    new (&this->expr_node) T(Fin::move(value));
  }
};

