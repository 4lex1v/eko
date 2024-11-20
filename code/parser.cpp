
#include "anyfin/arena.hpp"
#include "anyfin/seq.hpp"
#include "anyfin/result.hpp"

#include "ast.hpp"
#include "parser.hpp"
#include "tokens.hpp"

using namespace Fin;

static constexpr usize memory_reservation_per_process = megabytes(1);

static bool operator == (const Token *token, Token_Kind kind) {
  return token->kind == kind;
}

static bool is_whitespace (char value) {
  return value == ' ' || value == '\t' || value == '\r' || value == '\n';
}

static bool is_numeric (u8 value) {
  return value >= '0' && value <= '9';
}

static bool is_alpha (u8 value) {
  return (value >= 'a' && value <= 'z') || (value >= 'A' && value <= 'Z');
}

static bool is_hex_digit (u8 c) {
  return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') ||
         (c >= 'A' && c <= 'F');
}

struct Tokenizer {
  const char *buffer;
  const char *current { buffer };

  u32 row    = 0;
  u32 column = 0;

  Tokenizer (const char *_buffer): buffer { _buffer } {}

  Array<Token> get_tokens (Memory_Arena &arena) {
    Array tokens { get_memory_at_current_offset<Token>(arena), 0 };

    while (true) {
      auto *token = new (arena) Token(read_token());
      tokens.count += 1;
      if (token->kind == Token::Last) break;
    }

    return tokens;
  }

  /*
    Tokenizer Functions
  */

  Token make_token (Token_Kind kind, String value = {}) const {
    return Token(kind, value, row, column);
  }

  /*
    Returns true if the last symbol in the file is reached, i.e \0
  */
  bool advance (u32 by = 1) {
    for (u32 i = 0; i < by; i++) {
      if (*current == '\0') return true;
      if (*current == '\n') {
        row += 1;
        column = 0;
      }

      current++;
    }

    return false;
  }

  bool looking_at (u8 value) const { return *current == value; }

  void skip_basic_whitespace () {
    while (*current == ' ' || *current == '\t')
      current += 1;
  }

  Token read_symbolic_token () {
    fin_ensure(looking_at('_') || is_alpha(*current));

    const char *value_start = current;
    while (true) {
      if (*current == '_' || is_alpha(*current) || is_numeric(*current)) {
        advance();
        continue;
      }

      break;
    }

    auto value = String(value_start, current - value_start);
    if (value == "return") return make_token(Token::Return);
    if (value == "struct") return make_token(Token::Struct);
    if (value == "use")    return make_token(Token::Use);

    return make_token(Token::Symbol, value);
  }

  Token read_numeric_token () {
    fin_ensure(is_numeric(*current));

    auto value_start = current;

    // Check for hexadecimal, binary, or octal literals
    if (*current == '0') {
      advance();
      if (*current == 'x' || *current == 'X') {
        advance();
        while (is_hex_digit(*current)) {
          advance();
        }
      } else if (*current == 'b' || *current == 'B') {
        advance();
        while (*current == '0' || *current == '1') {
          advance();
        }
      } else if (is_numeric(*current)) {
        while (*current >= '0' && *current <= '7') {
          advance();
        }
      } else {
        // Single digit '0' literal
      }
    } else {
      // Decimal literals
      while (is_numeric(*current)) {
        advance();
      }
    }

    return make_token(Token::Numeric, String(value_start, current - value_start));
  }

  Token read_string_literal () {
    fin_ensure(looking_at('"'));

    auto value_start = current;
    if (advance()) {
      // skip the opening " of a string literal
      fin_ensure(false && "Handle error with unclosed string literal");
      return {};
    }

    /*
      TODO: This need a proper function that handles a string literal with all
      the escapes and other stuff
    */
    while (true) {
      auto value = *current;
      if (value == '"') {
        advance();
        break;
      }

      advance();
    }

    /*
      At this point the tokenizer is one symbol after the string literal closing
      quote, so no additional advances are needed.
    */

    auto token_value = String(value_start, current - value_start);
    return make_token(Token::String_Literal, token_value);
  }

  Token read_token () {
    skip_basic_whitespace();

    if (looking_at('\0')) return make_token(Token::Last);

    if (looking_at('\r')) {
      advance();
      fin_ensure(looking_at('\n'));
    }

    if (looking_at('\n')) {
      auto token = make_token(Token::Newline);
      advance();
      return token;
    }

    if (looking_at('_') || is_alpha(*current)) return read_symbolic_token();
    if (is_numeric(*current))                  return read_numeric_token();
    if (looking_at('"'))                       return read_string_literal();

    Token result {};
    switch (*current) {
      case ':': result = make_token(Token::Colon, ":");                break;
      case '(': result = make_token(Token::Open_Round_Bracket, "(");   break;
      case ')': result = make_token(Token::Close_Round_Bracket, ")");  break;
      case '[': result = make_token(Token::Open_Square_Bracket, "[");  break;
      case ']': result = make_token(Token::Close_Square_Bracket, "]"); break;
      case '{': result = make_token(Token::Open_Curly_Bracket, "{");   break;
      case '}': result = make_token(Token::Close_Curly_Bracket, "}");  break;
      case ',': result = make_token(Token::Coma, ",");                 break;
      case ';': result = make_token(Token::Semicolon, ";");            break;
      case '=': result = make_token(Token::Equal, "=");                break;
      case '.': result = make_token(Token::Period, ".");               break;
      case '+': result = make_token(Token::Plus, "+");                 break;
      case '-': result = make_token(Token::Minus, "-");                break;
      case '*': result = make_token(Token::Star, "*");                 break;
      case '_': result = make_token(Token::Underscore, "_");           break;

      default: fin_ensure(false && "Unsupported character symbol");
    }

    advance();

    return result;
  }
};

struct Parser {
  Memory_Arena &arena;

  const Array<Token> &tokens;
  const Token        *current { tokens.values };

  Parser (Memory_Arena &_arena, const Array<Token> &_tokens):
    arena  { _arena },
    tokens { _tokens }
  {}

  /*
    Parser Functions.
  */

  bool looking_at (Token_Kind kind, bool ignore_new_line_token = true) {
    if (ignore_new_line_token)
      skip_new_line_tokens();
    return current == kind;
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

  void consume (Token_Kind kind, bool ignore_new_line_token = true) {
    // TODO: This should be a parsing error
    fin_ensure(current == kind);
    advance(1, ignore_new_line_token);
  }

  /*
    Returns true if the end of the token's stream was reached.
  */
  bool advance (u32 by = 1, bool ignore_new_line_token = true) {
    for (u32 i = 0; i < by; i++) {
      if (looking_at(Token::Last, ignore_new_line_token)) return true;
      current++;
    }

    return false;
  }

  void rewind (u32 by = 1, bool ignore_new_line_token = true) {
    fin_ensure((current - by) >= tokens.values);

    for (u32 i = by; i > 0; i--) {
      current--;
      if (current == Token::Newline && ignore_new_line_token)
        i += 1;
    }
  }

  void skip_new_line_tokens () {
    while (current == Token::Newline)
      current++;
  }

  template <typename T> Node *       new_node      (T &&value) { return new (arena) Node(forward<T>(value)); }
  template <typename T> Type_Node *  new_type_node (T &&value) { return new (arena) Type_Node(forward<T>(value)); }

#define parse_until(TOKEN)                                                     \
  while (!looking_at(TOKEN) && !looking_at(Token::Last))

  Type_Node parse_type () {
    if (looking_at(Token::Star)) {
      advance();
      return Pointer_Type_Node(new_type_node(parse_type()));
    }

    if (looking_at(Token::Open_Square_Bracket)) {
      advance();

      if (looking_at(Token::Close_Square_Bracket)) {
        advance();
        return Seq_Type_Node(new_type_node(parse_type()));
      }

      Expression_Node bounds_argument = parse_expression();
      consume(Token::Close_Square_Bracket);
      auto elem_type = parse_type();

      return Array_Type_Node(move(bounds_argument), elem_type);
    }

    if (looking_at(Token::Symbol)) {
      auto plain_type = Plain_Type_Node(*current);
      advance();

      if (looking_at(Token::Open_Round_Bracket)) {
        advance();

        //Array param(get_memory_at_current_offset<Parameter_Node>(arena), 0);
        parse_until(Token::Close_Round_Bracket) {
          fin_ensure(false && "Complete this section");
          // plain_type->parameters.push_back(parse_type());
          // if (looking_at(Token::Coma)) advance();
        }

        advance();
      }

      return new_type_node(move(plain_type));
    }

    fin_ensure(false && "Unexpected symbol in the type");

    return nullptr;
  }

  // Type_Node * parse_type () {
  //   if (looking_at(Token::Star)) {
  //     advance();
  //     return new_type_node(Pointer_Type_Node(parse_type()));
  //   }

  //   if (looking_at(Token::Open_Square_Bracket)) {
  //     advance();

  //     if (looking_at(Token::Close_Square_Bracket)) {
  //       advance();
  //       return new_type_node(Seq_Type_Node(parse_type()));
  //     }

  //     auto bounds_argument = parse_expression();
  //     consume(Token::Close_Square_Bracket);
  //     auto elem_type = parse_type();

  //     return new_type_node(Array_Type_Node(bounds_argument, elem_type));
  //   }

  //   if (looking_at(Token::Symbol)) {
  //     auto plain_type = Plain_Type_Node(*current);
  //     advance();

  //     if (looking_at(Token::Open_Round_Bracket)) {
  //       advance();

  //       //Array param(get_memory_at_current_offset<Parameter_Node>(arena), 0);
  //       parse_until(Token::Close_Round_Bracket) {
  //         fin_ensure(false && "Complete this section");
  //         // plain_type->parameters.push_back(parse_type());
  //         // if (looking_at(Token::Coma)) advance();
  //       }

  //       advance();
  //     }

  //     return new_type_node(move(plain_type));
  //   }

  //   fin_ensure(false && "Unexpected symbol in the type");

  //   return nullptr;
  // }

  Variable_Node parse_parameter () {
    auto param_name = *current;
    advance();

    consume(Token::Colon);

    if (!looking_at(Token::Equal)) {
      auto var_type = new (arena) Type_Node(parse_type());
      return Variable_Node(param_name, var_type);
    }

    auto init_expr = new (arena) Expression_Node(parse_expression());

    return Variable_Node(param_name, nullptr, init_expr);
  }

  template <typename T>
  inline List<T> parse_sequence_within(Token_Kind open, Token_Kind close, T (Parser::*func)(void)) {
    List<T> nodes;
    if (!looking_at(open)) return nodes;

    consume(open);
    parse_until(close) {
      list_push(arena, nodes, (this->*func)());
    }
    consume(close);
    
    return nodes;
  }

  List<Declaration_Node> parse_parameter_list () {
    List<Declaration_Node> params;

    if (!looking_at(Token::Open_Round_Bracket)) return params;

    consume(Token::Open_Round_Bracket);

    parse_until(Token::Close_Round_Bracket) {
      list_push(arena, params, parse_parameter());
      if (looking_at(Token::Coma)) advance();
    }
    
    consume(Token::Close_Round_Bracket);

    return params;
  }

  Expression_Node parse_unary_expression () {
    if (looking_at(Token::String_Literal) || looking_at(Token::Numeric)) {
      auto literal = Literal_Node(*current);
      advance();
      return new_node(literal);
    }

    if (!looking_at(Token::Symbol)) {
      // TODO: Implement
      fin_ensure(false && "Unsupported expression kind");

      return nullptr;
    }

    fin_ensure(looking_at(Token::Symbol));

    Expression_Node node = Identifier_Node(*current);
    advance();

    if (looking_at(Token::Period)) {
      advance();

      while (true) {
        skip_new_line_tokens();
        // TODO: This should be a parser error 
        fin_ensure(current == Token::Symbol);

        node = Member_Access_Node {
          .expr   = new (arena) Expression_Node(move(node)),
          .member = *current
        };

        advance();

        if (looking_at(Token::Period)) {
          advance();
          continue;
        }

        break;
      }
    }

    if (looking_at(Token::Open_Round_Bracket)) {
      advance();

      auto args = parse_sequence_within(Token::Open_Round_Bracket, Token::Close_Round_Bracket, &Parser::parse_expression);

      return Function_Call_Node(node, move(args));
    }

    return node;
  }

  Expression_Node parse_expression () {
    auto left_side = parse_unary_expression();

    if (looking_at(Token::Plus)) {
      advance();

      auto right_side = parse_unary_expression();
    }
  }

  Declaration_Node parse_declaration () {
    if (!looking_at(Token::Symbol)) {
      // TODO: Process error
      fin_ensure(false && "ERROR: Invalid declaration syntax");
      return nullptr;
    }

    auto name_token = *current;
    advance();

    consume(Token::Colon);

    if (!(looking_at(Token::Colon) || looking_at(Token::Equal))) {
      // TODO: Parsing error
      fin_ensure(false && "Parsing error");
      return nullptr;
    }

    const auto is_constant = looking_at(Token::Colon);

    advance();

    if (looking_at(Token::Open_Round_Bracket)) {
      if (!is_constant) {
        fin_ensure(false && "Parsing error");
        return nullptr;
      }

      auto lambda_params = parse_parameter_list();

      Type_Node *return_type = nullptr;
      if (!looking_at(Token::Open_Curly_Bracket)) {
        return_type = parse_type();
      }

      fin_ensure(current == Token::Open_Curly_Bracket);
      auto lambda_body = parse_sequence_within(Token::Open_Curly_Bracket, Token::Close_Curly_Bracket, &Parser::parse_next);

      return Lambda_Node(name_token, move(lambda_params), return_type, move(lambda_body));
    }

    if (looking_at(Token::Struct)) {
      advance();

      return Struct_Node {
        .params = parse_parameter_list(),
        .fields = parse_sequence_within(Token::Open_Curly_Bracket, Token::Close_Curly_Bracket, &Parser::parse_parameter),
      };
    }

    auto declaration_value = parse_expression();

    return Constant_Node(name_token, declaration_value);
  }

  Node * parse_next () {
    if (looking_at(Token::Symbol)) {
      advance();

      if (looking_at(Token::Colon)) {
        advance();

        if (!(looking_at(Token::Colon) || looking_at(Token::Equal))) {
          // TODO: This should be a parsing error
          fin_ensure(false && "Unexpected token");

          return nullptr;
        }

        rewind(2);

        return new_node(parse_declaration());
      }

      rewind();

      return parse_expression();
    }

    if (looking_at(Token::Return)) {
      advance();

      auto return_expr = parse_expression();

      return new_node(Return_Node(return_expr));
    }

    fin_ensure(false && "Unsupported token");

    return nullptr;
  }

  List<Node *> parse_tree () {
    List<Node *> nodes { arena };

    while (!looking_at(Token::Last)) {
      list_push(nodes, parse_next());
    }

    return nodes;
  }
};

List<Node *> build_tree (Memory_Arena &arena, const char *buffer) {
  auto tokenizer = Tokenizer(buffer);
  auto tokens    = tokenizer.get_tokens(arena);

  // TODO: handle as error cases
  fin_ensure(tokens.count > 1);
  fin_ensure(tokens[tokens.count - 1].kind == Token::Last);
  
  auto parser = Parser(arena, tokens);
  return parser.parse_tree();
}
