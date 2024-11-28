
#include "anyfin/arena.hpp"
#include "anyfin/result.hpp"

#include "eko.hpp"
#include "ast.hpp"
#include "parser.hpp"
#include "tokens.hpp"
#include "utils.hpp"

using Fin::Memory_Arena;
using Fin::Array;
using Fin::List;

static constexpr usize memory_reservation_per_process = Fin::megabytes(1);

template <typename T> using Result = Fin::Result<Parser_Error, T>;

struct Parser {
  Memory_Arena &arena;

  Source_File &unit;
  const Token *current { unit.tokens.values };

  /*
    Parser Functions.
  */

  bool looking_at (Token_Kind kind, bool ignore_new_line_token = true) {
    if (ignore_new_line_token) skip_new_line_tokens();
    return current == kind;
  }

  bool eat (Token_Kind kind, bool ignore_new_line_token = true) {
    if (!looking_at(kind, ignore_new_line_token)) return false;
    (void) advance(ignore_new_line_token);
    return true;
  }

  bool check_next (Token_Kind kind, bool ignore_new_line_tokens = true) {
    auto local = current;
    if (ignore_new_line_tokens) {
      while (local == Token::Newline)
        local++;
    }

    local++;

    if (ignore_new_line_tokens) {
      while (local == Token::Newline)
        local++;
    }

    return local == kind;
  }

  /*
    Returns false if the end of the token's stream was reached.
  */
  [[nodiscard]] bool advance (u32 by = 1, bool ignore_new_line_token = true) {
    for (u32 i = 0; i < by; i++) {
      if (looking_at(Token::Last, ignore_new_line_token)) return false;
      current++;
    }

    return true;
  }

  void rewind (u32 by = 1, bool ignore_new_line_token = true) {
    fin_ensure((current - by) >= unit.tokens.values);

    for (u32 i = by; i > 0; i--) {
      current--;
      if (current == Token::Newline && ignore_new_line_token)
        i += 1;
    }
  }

  void skip_new_line_tokens () {
    while (current == Token::Newline || current == Token::CR)
      current++;
  }

  /*
    Errors
   */
  Parser_Error unexpected_token (Token_Kind expected_kind) {
    return Parser_Error {
      .kind = Parser_Error::Unexpected_Token,
    };
  }

#define ensure_token(KIND) if (!looking_at(KIND)) return unexpected_token(KIND)

  /*
    Functions
   */
#define parse_until(TOKEN)                                \
  while (!looking_at(TOKEN) && !looking_at(Token::Last))

  template <typename To = void, typename From>
  auto new_node (From &&value) {
    using Raw = Fin::remove_ref<From>;
    if constexpr (Fin::same_types<To, void>) {
      return new (arena) Raw(Fin::move(value));
    }
    else {
      return new (arena) To(Raw(Fin::move(value)));
    }
  }

  template <typename T>
  Result<List<T>> parse_sequence_within(Token_Kind open, Token_Kind close, Result<T> (Parser::*func) ()) {
    List<T> nodes;

    if (eat(open)) {
      parse_until(close) {
        try(value, (this->*func)());
        list_push(arena, nodes, Fin::move(value));
      }

      if (!eat(close)) return unexpected_token(close);
    }
    
    return nodes;
  }

  Result<Type_Node> parse_type () {
    if (eat(Token::Star)) {
      try(element_type, parse_type());
      return Type_Node(Pointer_Type_Node(new_node<Type_Node>(element_type)));
    }

    if (eat(Token::Open_Square_Bracket)) {
      if (eat(Token::Close_Square_Bracket)) {
        try(element_type, parse_type());
        return Type_Node(Seq_Type_Node(new_node<Type_Node>(element_type)));
      }

      try(bounds_argument, parse_expression());
      if (!eat(Token::Close_Square_Bracket)) return unexpected_token(Token::Close_Square_Bracket);

      try(element_type, parse_type());

      return Type_Node(Array_Type_Node(new_node<Type_Node>(element_type), bounds_argument));
    }

    if (!looking_at(Token::Symbol)) return unexpected_token(Token::Symbol);

    auto plain_type = Plain_Type_Node(*current);
    if (!advance()) return unexpected_token(Token::Last);

    try(type_params, parse_sequence_within(Token::Open_Round_Bracket, Token::Close_Round_Bracket, &Parser::parse_type));
    plain_type.parameters = type_params;

    return Type_Node(plain_type);
  }

  Result<Variable_Node> parse_parameter () {
    auto param_name = *current;
    if (!advance()) return unexpected_token(Token::Last);

    if (!eat(Token::Colon)) return unexpected_token(Token::Colon);

    if (!looking_at(Token::Equal)) {
      try(var_type, parse_type())
      return Variable_Node(param_name, new_node(var_type));
    }

    try(init_expr, parse_expression());

    return Variable_Node(param_name, nullptr, new_node(init_expr));
  }

  Result<usize> parse_integer (const Token &token) {
    return 0;
  }
  
  Result<Expression_Node> parse_unary_expression () {
    auto is_signed = eat(Token::Minus);

    if (looking_at(Token::Integer_Literal)) {
      auto literal = Literal_Node { .value = *current };

      try(int_value, parse_integer(*current));
      
      (void) advance();

      return Expression_Node(literal);
    }

    if (looking_at(Token::String_Literal)) {
      auto literal = Literal_Node { .value = *current, .string_value = current->value };
      (void) advance();
      return Expression_Node(literal);
    }

    ensure_token(Token::Symbol);

    Expression_Node node = Identifier_Node(*current);
    if (!advance()) return unexpected_token(Token::Last);

    while (eat(Token::Period)) {
      ensure_token(Token::Symbol);

      node = Expression_Node(Member_Access_Node {
        .expr   = new_node(node),
        .member = *current
      });

      (void) advance();
    }

    if (eat(Token::Open_Round_Bracket)) {
      try(args, parse_sequence_within(Token::Open_Round_Bracket, Token::Close_Round_Bracket, &Parser::parse_expression));
      return Expression_Node(Function_Call_Node(new_node(node), args));
    }

    return node;
  }

  Result<Expression_Node> parse_expression () {
    try(left_side, parse_unary_expression());

    if (eat(Token::Plus)) {
      try(right_side, parse_unary_expression());
      return Expression_Node(Binary_Expr_Node(new_node(left_side), new_node(right_side)));
    }

    return left_side;
  }

  Result<Declaration_Node> parse_field () {
    fin_ensure(looking_at(Token::Symbol));

    auto field_name = *current;
    ensure_token(Token::Colon);

    if (eat(Token::Colon)) {
      try(init_expr, parse_expression());
      return Declaration_Node(Constant_Node(field_name, init_expr));
    }

    Type_Node *field_type = nullptr;
    if (!looking_at(Token::Equal)) {
      try(type_value, parse_type());
      field_type = new_node(type_value);
    }

    if (!looking_at(Token::Equal)) {
      return Declaration_Node(Variable_Node(field_name, field_type, nullptr));
    }

    ensure_token(Token::Equal);
    
    try(init_expr, parse_expression());

    return Declaration_Node(Variable_Node(field_name, field_type, new_node(init_expr)));
  }

  Result<Declaration_Node> parse_declaration () {
    ensure_token(Token::Symbol);

    auto name_token = *current;
    if (!advance()) return unexpected_token(Token::Last);

    if (!eat(Token::Colon)) return unexpected_token(Token::Colon);

    if (looking_at(Token::Symbol)) {
      try(value_type, parse_type());

      Expression_Node *init_expr = nullptr;
      if (eat(Token::Equal)) {
        try(expr_value, parse_expression());
        init_expr = new_node(expr_value);
      }
      
      return Declaration_Node(Variable_Node(name_token, new_node(value_type), init_expr));
    }

    if (eat(Token::Equal)) {
      try(expr_value, parse_expression());
      return Declaration_Node(Variable_Node(name_token, nullptr, new_node(expr_value)));
    }

    if (!eat(Token::Colon)) return unexpected_token(Token::Colon);

    if (looking_at(Token::Open_Round_Bracket)) {
      try(lambda_params, parse_sequence_within(Token::Open_Round_Bracket, Token::Close_Round_Bracket, &Parser::parse_parameter));

      Type_Node *return_type = nullptr;
      if (!looking_at(Token::Open_Curly_Bracket)) {
        try(type_value, parse_type());
        return_type = new_node(type_value);
      }

      try(lambda_body, parse_sequence_within(Token::Open_Curly_Bracket, Token::Close_Curly_Bracket, &Parser::parse_next));

      return Declaration_Node(Lambda_Node(name_token, lambda_params, lambda_body, return_type));
    }

    if (eat(Token::Struct)) {
      try(params, parse_sequence_within(Token::Open_Round_Bracket, Token::Close_Round_Bracket, &Parser::parse_parameter));
      try(fields, parse_sequence_within(Token::Open_Curly_Bracket, Token::Close_Curly_Bracket, &Parser::parse_field));

      return Declaration_Node(Struct_Node(name_token, params, fields));
    }

    try(declaration_value, parse_expression());

    return Declaration_Node(Constant_Node(name_token, declaration_value));
  }

  Result<Node> parse_next () {
    if (eat(Token::Symbol)) {
      if (eat(Token::Colon)) {
        rewind(2);
        try(decl_node, parse_declaration());
        return Node(decl_node);
      }

      rewind();
      try(expr_node, parse_expression());
      return Node(expr_node);
    }

    if (eat(Token::Return)) {
      try(return_expr, parse_expression());
      return Node(Statement_Node(Return_Node(return_expr)));
    }

    // TODO: This is a fake error for now
    fin_ensure(false && "INCOMPLETE");
    return unexpected_token(Token::Symbol);
  }

  Fin::Option<Parser_Error> parse_tree () {
    while (!looking_at(Token::Last)) {
      try(node, parse_next());
      list_push(arena, unit.tree, Fin::move(node));
    }

    return Fin::None();
  }
};

Fin::Option<Parser_Error> build_tree (Memory_Arena &arena, Source_File &unit) {
  auto parser = Parser(arena, unit);
  return parser.parse_tree();
}
