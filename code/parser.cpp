
#include "anyfin/arena.hpp"
#include "anyfin/seq.hpp"

#include "parser.hpp"
#include "tokens.hpp"

using namespace Fin;

constexpr usize memory_reservation_per_process = megabytes(1);

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
  Memory_Arena arena;
  
  Array<Token> tokens;
  const Token  *current = tokens.values;

  Parser (const char *buffer)
    : arena { reserve_virtual_memory(memory_reservation_per_process) }
  {
    tokens = Tokenizer(buffer).get_tokens(arena);

    fin_ensure(tokens.count > 1);
    fin_ensure(tokens[tokens.count].kind == Token::Last);
  }

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

#define parse_until(TOKEN)                                                     \
  while (!looking_at(TOKEN) && !looking_at(Token::Last))

  Type_Node *parse_type () {
    if (looking_at(Token::Star)) {
      advance();
      return new (arena) Pointer_Type_Node(parse_type());
    }

    if (looking_at(Token::Open_Square_Bracket)) {
      advance();

      if (looking_at(Token::Close_Square_Bracket)) {
        advance();
        return new (arena) Seq_Type_Node(parse_type());
      }

      auto bounds_argument = parse_expression();
      consume(Token::Close_Square_Bracket);
      auto elem_type = parse_type();

      return new (arena) Array_Type_Node(bounds_argument, elem_type);
    }

    if (looking_at(Token::Symbol)) {
      auto plain_type = new (arena) Plain_Type_Node(*current);
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

      return plain_type;
    }

    fin_ensure(false && "Unexpected symbol in the type");

    return nullptr;
  }

  Parameter_Node parse_parameter () {
    Parameter_Node param_node;

    param_node.name = *current;
    advance();

    consume(Token::Colon);

    if (looking_at(Token::Equal)) {
      // TODO: Should parse the default value initialization
      return param_node;
    }

    param_node.type = parse_type();

    if (looking_at(Token::Equal)) {
      // TODO: Parse default value initialization
    }

    return param_node;
  }

  template <typename T>
  inline List<T> parse_sequence_within(Token_Kind open, Token_Kind close, T (Parser::*func)(void)) {
    List<T> nodes { arena };
    if (!looking_at(open)) return nodes;

    consume(open);
    parse_until(close) {
      list_push(nodes, (this->*func)());
    }
    consume(close);
    
    return nodes;
  }

  List<Parameter_Node> parse_parameter_list () {
    List<Parameter_Node> params { arena };
    if (!looking_at(Token::Open_Round_Bracket)) return params;

    consume(Token::Open_Round_Bracket);

    parse_until(Token::Close_Round_Bracket) {
      list_push(params, parse_parameter());
      if (looking_at(Token::Coma)) advance();
    }
    
    consume(Token::Close_Round_Bracket);

    return params;
  }

  Node * parse_unary_expression () {
    if (looking_at(Token::String_Literal) || looking_at(Token::Numeric)) {
      auto node = new (arena) Literal_Node(*current);
      advance();
      return node;
    }

    if (!looking_at(Token::Symbol)) {
      // TODO: Implement
      fin_ensure(false && "Unsupported expression kind");

      return nullptr;
    }

    List<Token> id_list { arena };

    list_push_copy(id_list, *current);
    advance();

    if (looking_at(Token::Period)) {
      advance();
      while (true) {
        skip_new_line_tokens();
        fin_ensure(current == Token::Symbol);

        list_push_copy(id_list, *current);
        advance();

        if (looking_at(Token::Period)) {
          advance();
          continue;
        }

        break;
      }
    }

    auto id_node = new (arena) Identifier_Node(move(id_list));

    if (looking_at(Token::Open_Round_Bracket)) {
      advance();

      auto call_node = new (arena) Function_Call_Node;
      call_node->expr = id_node;
      call_node->args = parse_sequence_within(Token::Open_Round_Bracket, Token::Close_Round_Bracket, &Parser::parse_expression);

      return call_node;
    }

    return id_node;
  }

  Node * parse_expression () {
    return parse_unary_expression();
  }

  Node * parse_declaration () {
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

      auto lambda_decl = new (arena) Lambda_Decl_Node(name_token);
      lambda_decl->params = parse_parameter_list();

      if (!looking_at(Token::Open_Curly_Bracket)) {
        lambda_decl->return_type = parse_type();
      }

      fin_ensure(current == Token::Open_Curly_Bracket);
      lambda_decl->body = parse_sequence_within(Token::Open_Curly_Bracket, Token::Close_Curly_Bracket, &Parser::parse_next);

      return lambda_decl;
    }

    if (looking_at(Token::Struct)) {
      advance();

      auto struct_decl = new (arena) Struct_Decl_Node(name_token);
      struct_decl->params = parse_parameter_list();
      struct_decl->fields = parse_sequence_within(Token::Open_Curly_Bracket, Token::Close_Curly_Bracket, &Parser::parse_parameter);

      return struct_decl;
    }

    auto declaration_value = parse_expression();
    auto declaration_node = new (arena) Value_Decl_Node(name_token, declaration_value, is_constant);

    return declaration_node;
  }

  Node * parse_next () {
    if (looking_at(Token::Return)) {
      advance();

      auto return_expr = parse_expression();

      return new (arena) Return_Node(return_expr);
    }

    if (looking_at(Token::Symbol)) {
      advance();

      if (looking_at(Token::Colon)) {
        advance();

        if (!(looking_at(Token::Colon) || looking_at(Token::Equal))) {
          fin_ensure(false && "Unexpected token");
          return nullptr;
        }

        rewind(2);

        return parse_declaration();
      }

      rewind();
      return parse_expression();
    }

    fin_ensure(false && "Unsupported token");

    return nullptr;
  }
};

Root_Node build_tree (const char *buffer) {
  Parser parser { buffer };
  
  Root_Node root;

  while (true) {
    if (parser.looking_at(Token::Use)) {
      fin_ensure(false && "Support for use statements");
      break;
    }

    if (parser.looking_at(Token::Symbol)) {
      list_push(root.nodes, parser.parse_declaration());
      continue;
    }

    if (parser.looking_at(Token::Last)) break;

    fin_ensure(false && "Unsupported token");
  }

  return root;
}

