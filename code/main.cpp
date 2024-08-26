
#include <cassert>
#include <cstdlib>
#include <cstdio>

#include <string>

#include "anyfin/base.hpp"
#include "anyfin/prelude.hpp"
#include "anyfin/arena.hpp"
#include "anyfin/array.hpp"
#include "anyfin/defer.hpp"
#include "anyfin/list.hpp"
#include "anyfin/memory.hpp"
#include "anyfin/seq.hpp"
#include "anyfin/strings.hpp"
#include "anyfin/option.hpp"

using namespace Fin;

struct Loaded_File {
  u8 *content;
  usize size;
};

Loaded_File read_file_into_memory (const char *file_name) {
  auto file = fopen(file_name, "rb");
  if (file == nullptr) return {};
  defer { fclose(file); };

  fseek(file, 0, SEEK_END);
  usize size = ftell(file);
  fseek(file, 0, SEEK_SET);

  auto buffer = (u8 *) malloc(size + 1);
  if (buffer == nullptr) return {};

  usize bytes_read = fread(buffer, 1, size, file);
  if (bytes_read != size) {
    free(buffer);
    return {};
  }

  buffer[size] = '\0';

  return {
    .content = buffer,
    .size    = size
  };
}

struct Token {
  enum Kind {
    Undefined,
    Last,

    Colon,
    Open_Round_Bracket,
    Close_Round_Bracket,
    Open_Square_Bracket,
    Close_Square_Bracket,
    Open_Curly_Bracket,
    Close_Curly_Bracket,
    Coma,
    Semicolon,
    Equal,
    Period,
    Plus,
    Minus,
    Star,
    Underscore,

    // Keywords
    Struct,
    Return,
    Use,
    And,
    Or,

    Newline,
    Numeric, // This should be refined into different types of literals supported, integers and floats at least
    String_Literal,
    Symbol,
  };

  Kind kind = Undefined;
  String value;
  u32 row, column;

  bool operator == (Token::Kind kind) const { return this->kind == kind; }
  bool operator == (String value)     const { return this->value == value; }
};

static bool operator == (const Token *token, Token::Kind kind) {
  return token->kind == kind;
}

struct File_Tokenizer {
  Memory_Arena &arena;

  const u8    *file_buffer;
  const u8    *current;
  const usize  file_buffer_size;

  u32 row    = 0;
  u32 column = 0;

  File_Tokenizer (Memory_Arena &_arena, const Loaded_File &file)
    : arena            { _arena },
      file_buffer      { file.content },
      current          { file_buffer },
      file_buffer_size { file.size }
    {}

  /*
    Tokenizer Functions
  */

  Token make_token (Token::Kind kind, String value = {}) {
    return Token(kind, value, row, column);
  }

  /*
    Returns true if the last symbol in the file is reached, i.e \0
  */
  bool advance (u32 by = 1) {
    for (u32 i = 0; i < by; i++) {
      if (*current == '\0') return true;
      if (*current == '\n') {
        row    += 1;
        column  = 0;
      }

      current++;
    }

    return false;
  }

  bool looking_at (u8 value) {
    return *current == value;
  }

  void skip_basic_whitespace () {
    while (*current == ' ' || *current == '\t') current += 1;
  }

};

static bool is_whitespace (u8 value) {
  return value == ' ' || value == '\t' || value == '\r' || value == '\n';
}

static bool is_numeric (u8 value) {
  return value >= '0' && value <= '9';
}

static bool is_alpha (u8 value) {
  return (value >= 'a' && value <= 'z') ||
    (value >= 'A' && value <= 'Z');
}

static bool is_hex_digit(u8 c) {
  return (c >= '0' && c <= '9') || 
    (c >= 'a' && c <= 'f') || 
    (c >= 'A' && c <= 'F');
}

static Token read_symbolic_token (File_Tokenizer &tokenizer) {
  assert(tokenizer.looking_at('_') || is_alpha(*tokenizer.current));

  const u8 *value_start = tokenizer.current;
  while (true) {
    if (*tokenizer.current == '_' ||
        is_alpha(*tokenizer.current) ||
        is_numeric(*tokenizer.current)) {
      tokenizer.advance();
      continue;
    }

    break;
  }

  auto string_view = String(value_start, tokenizer.current - value_start);
  if      (string_view == "return") return tokenizer.make_token(Token::Return);
  else if (string_view == "struct") return tokenizer.make_token(Token::Struct);
  else if (string_view == "use")    return tokenizer.make_token(Token::Use);

  return tokenizer.make_token(Token::Symbol, copy_string(tokenizer.arena, string_view));
}

static Token read_numeric_token (File_Tokenizer &stream) {
  assert(is_numeric(*stream.current));

  auto value_start = stream.current;

  // Check for hexadecimal, binary, or octal literals
  if (*stream.current == '0') {
    stream.advance();
    if (*stream.current == 'x' || *stream.current == 'X') {
      stream.advance();
      while (is_hex_digit(*stream.current)) {
        stream.advance();
      }
    } else if (*stream.current == 'b' || *stream.current == 'B') {
      stream.advance();
      while (*stream.current == '0' || *stream.current == '1') {
        stream.advance();
      }
    } else if (is_numeric(*stream.current)) {
      while (*stream.current >= '0' && *stream.current <= '7') {
        stream.advance();
      }
    } else {
      // Single digit '0' literal
    }
  } else {
    // Decimal literals
    while (is_numeric(*stream.current)) {
      stream.advance();
    }
  }

  return stream.make_token(Token::Numeric, copy_string(stream.arena, value_start, stream.current - value_start));
}

static Token read_string_literal (File_Tokenizer &tokenizer) {
  assert(tokenizer.looking_at('"'));

  auto value_start = tokenizer.current;
  if (tokenizer.advance()) { // skip the opening " of a string literal
    assert(false && "Handle error with unclosed string literal");
    return {};
  }

  /*
    TODO: This need a proper function that handles a string literal with all the escapes and other stuff
  */
  while (true) {
    auto value = *tokenizer.current;
    if (value == '"') {
      tokenizer.advance();
      break;
    }

    tokenizer.advance();
  }

  /*
    At this point the tokenizer is one symbol after the string literal closing quote,
    so no additional advances are needed.
  */
  
  auto value = copy_string(tokenizer.arena, value_start, tokenizer.current - value_start);
  return tokenizer.make_token(Token::String_Literal, value);
}

static Token read_token (File_Tokenizer &tokenizer) {
  tokenizer.skip_basic_whitespace();

  if (tokenizer.looking_at('\0'))
    return { .kind = Token::Last };

  if (tokenizer.looking_at('\r')) {
    tokenizer.advance();
    assert(tokenizer.looking_at('\n'));
  }
  if (tokenizer.looking_at('\n')) {
    auto token = tokenizer.make_token(Token::Newline);
    tokenizer.advance();
    return token;
  }

  if (tokenizer.looking_at('_') || is_alpha(*tokenizer.current))
    return read_symbolic_token(tokenizer);
    
  if (is_numeric(*tokenizer.current))
    return read_numeric_token(tokenizer);

  if (tokenizer.looking_at('"')) {
    return read_string_literal(tokenizer);
  }
  
  Token result {};
  switch (*tokenizer.current) {
    case ':': result = tokenizer.make_token(Token::Colon,                ":"); break;
    case '(': result = tokenizer.make_token(Token::Open_Round_Bracket,   "("); break;
    case ')': result = tokenizer.make_token(Token::Close_Round_Bracket,  ")"); break;
    case '[': result = tokenizer.make_token(Token::Open_Square_Bracket,  "["); break;
    case ']': result = tokenizer.make_token(Token::Close_Square_Bracket, "]"); break;
    case '{': result = tokenizer.make_token(Token::Open_Curly_Bracket,   "{"); break;
    case '}': result = tokenizer.make_token(Token::Close_Curly_Bracket,  "}"); break;
    case ',': result = tokenizer.make_token(Token::Coma,                 ","); break;
    case ';': result = tokenizer.make_token(Token::Semicolon,            ";"); break;
    case '=': result = tokenizer.make_token(Token::Equal,                "="); break;
    case '.': result = tokenizer.make_token(Token::Period,               "."); break;
    case '+': result = tokenizer.make_token(Token::Plus,                 "+"); break;
    case '-': result = tokenizer.make_token(Token::Minus,                "-"); break;
    case '*': result = tokenizer.make_token(Token::Star,                 "*"); break;
    case '_': result = tokenizer.make_token(Token::Underscore,           "_"); break;
    default: assert(false && "Unsupported character symbol");
  }

  tokenizer.advance();

  return result;
}

static usize read_tokens (File_Tokenizer &tokenizer, Token *tokens_buffer) {
  usize count = 0;

  while (true) {
    auto token   = read_token(tokenizer);
    auto is_last = token.kind == Token::Last;
    tokens_buffer[count++] = std::move(token);

    if (is_last) break;
  }

  return count;
}

/*
  Parser
*/

struct Node;
struct Type_Node;

struct Pointer_Type_Node {
  Type_Node *pointee_type;
};

struct Seq_Type_Node {
  Type_Node *elements_type;
};

struct Array_Type_Node {
  Node      *bounds_expression;
  Type_Node *elements_type;
};

struct Plain_Type_Node {
  Token type_name;
  Type_Node* parameters[16];
  u32 parameters_count;
};

struct Type_Node {
  enum Kind {
    Pointer,
    Array,
    Seq,
    Plain
  };

  Kind kind;
  union {
    Pointer_Type_Node pointer_type;
    Array_Type_Node   array_type;
    Seq_Type_Node     seq_type;
    Plain_Type_Node   plain_type;
  };
};

struct Parameter_Node {
  Token name;
  Type_Node *type;
};

struct Struct_Decl_Node {
  Token name;
  List<Parameter_Node> params;
  List<Parameter_Node> fields;
};

struct Constant_Decl_Node {
  
};

struct Lambda_Decl_Node {
  Token name;

  List<Parameter_Node> params;
  Type_Node            return_type;

  List<Node*> body;
};

struct Function_Call_Node {
  Node         *expr;
  List<Node *>  args;
};

struct Node {
  enum Kind {
    Undefined,

    Root,
    Decl_Constant_Value,
    Decl_Variable_Value,
    Decl_Lambda,
    Decl_Struct,

    Identifier,
    Literal_String,
    Literal_Numeric,

    Function_Call,
  };

  Kind kind = Undefined;

  union {
    /*
      Value for both constant and variable values.
     */
    Node *expr_value;

    /*
      Value for any literal type like strings or numeric.
     */
    Token literal;

    /*
      Identifier expression could be however long, consisting of multiple parts
     */
    List<Token> identifier;

    Struct_Decl_Node struct_decl;
    Lambda_Decl_Node lambda_decl;

    Function_Call_Node call;
  };
};

struct Parser {
  Memory_Arena &arena;

  const Token *tokens;
  const Token *current; 

  Parser (Memory_Arena &_arena, Token *_tokens)
    : arena   { _arena },
      tokens  { _tokens },
      current { tokens }
    {}

  Parser &parser = *this;

  /*
    Parser Functions.
  */

  bool looking_at (Token::Kind kind, bool ignore_new_line_token = true) {
    if (ignore_new_line_token) skip_new_line_tokens();
    return current == kind;
  }

  bool check_next (Token::Kind kind, bool ignore_new_line_tokens = true) {
    auto local = current;
    if (ignore_new_line_tokens) {
      while (local == Token::Newline) local++;
    }

    local++;

    if (ignore_new_line_tokens) {
      while (local == Token::Newline) local++;
    }

    return local == kind;
  }
  
  void consume (Token::Kind kind, bool ignore_new_line_token = true) {
    // TODO: This should be a parsing error
    assert(current == kind);
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
      if (current == Token::Newline && ignore_new_line_token) i += 1;
    }
  }

  void skip_new_line_tokens () {
    while (current == Token::Newline) current++;
  }

#define parse_until(TOKEN) while (!looking_at(TOKEN) && !looking_at(Token::Last)) 

  void parse_type (Type_Node &type_node) {
    skip_new_line_tokens();
      
    switch (current->kind) {
      case Token::Star: {
        advance();
        
        type_node.kind = Type_Node::Pointer;
        type_node.pointer_type.pointee_type = reserve<Type_Node>(arena);

        parse_type(*type_node.pointer_type.pointee_type);

        return;
      }
      case Token::Open_Square_Bracket: {
        advance();

        if (looking_at(Token::Close_Square_Bracket)) {
          advance();

          type_node.kind = Type_Node::Seq;
          type_node.seq_type.elements_type = reserve<Type_Node>(arena);

          parse_type(*type_node.seq_type.elements_type);

          return;
        }

        type_node.kind = Type_Node::Array;
        type_node.array_type.elements_type     = reserve<Type_Node>(arena);
        type_node.array_type.bounds_expression = reserve<Node>(arena);

        parse_expression(*type_node.array_type.bounds_expression);
        consume(Token::Close_Square_Bracket);

        parse_type(*type_node.array_type.elements_type);

        return;
      }

      case Token::Symbol: {
        type_node.kind = Type_Node::Plain;

        auto &plain_type     = type_node.plain_type;
        plain_type.type_name = *current;

        advance();

        if (looking_at(Token::Open_Round_Bracket)) {
          advance();
          
          parse_until(Token::Close_Round_Bracket) {
            auto param_type_node =
              plain_type.parameters[plain_type.parameters_count++] =
              reserve<Type_Node>(arena);

            // TODO: Replace this with a dynamic array / linked list
            assert(plain_type.parameters_count <= 15);

            parse_type(*param_type_node);

            skip_new_line_tokens();
            if (current == Token::Coma) advance();
          }

          advance();
        }

        return;
      }

      default: {
        assert(false && "Unexpected symbol in the type");
      }
    }
  }

  void parse_parameter (Parameter_Node &param_node) {
    param_node.name = *current;
    advance();

    consume(Token::Colon);

    if (looking_at(Token::Equal)) {
      // TODO: Should parse the default value initialization
      return;
    }
    
    param_node.type = reserve<Type_Node>(arena);
    parse_type(*param_node.type);

    if (looking_at(Token::Equal)) {
      // TODO: Parse default value initialization
    }

    return;
  }

  void parse_parameter_list (List<Parameter_Node> &params_list) {
    consume(Token::Open_Round_Bracket);

    parse_until(Token::Close_Round_Bracket) {
      Parameter_Node param_node;
      parse_parameter(param_node);
      
      list_push(params_list, move(param_node));
    }

    advance(); // past the closing bracket
  }

  void parse_unary_expression (Node &expr_node) {
    skip_new_line_tokens();

    if (current == Token::String_Literal) {
      expr_node.kind  = Node::Literal_String;
      expr_node.literal = *current;

      advance();

      return;
    }

    if (current == Token::Numeric) {
      expr_node.kind  = Node::Literal_Numeric;
      expr_node.literal = *current;

      advance();
        
      return;
    }

    if (current == Token::Symbol) {
      auto id_list = List<Token>(arena);
      list_push_copy(id_list, *current);
      advance();

      if (looking_at(Token::Period)) {
        advance();
        while (true) {
          assert(current == Token::Symbol);

          list_push_copy(id_list, *current);
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
        
        expr_node.kind = Node::Function_Call;
        auto &call_node = expr_node.call;
        
        call_node.expr = reserve<Node>(arena);
        new (call_node.expr) Node {
          .kind       = Node::Identifier,
          .identifier = move(id_list)
        };

        call_node.args = List<Node *>(arena);
        parse_until(Token::Close_Round_Bracket) {
          auto *arg_node = reserve<Node>(arena);
          parse_expression(*arg_node);

          list_push(call_node.args, move(arg_node));

          skip_new_line_tokens();
          if (current == Token::Coma) advance();
        }

        consume(Token::Close_Round_Bracket);

        return;
      }

      expr_node.kind       = Node::Identifier;
      expr_node.identifier = move(id_list);

      return;
    }

    assert(false && "Unsupported expression kind");
  }

  void parse_expression (Node &expr_node) {
    parse_unary_expression(expr_node);
  }

  void parse_struct_body (Struct_Decl_Node &struct_decl) {
    consume(Token::Open_Curly_Bracket);

    struct_decl.fields = List<Parameter_Node>(arena);

    parse_until(Token::Close_Curly_Bracket) {
      Parameter_Node param_node {};
      parse_parameter(param_node);

      list_push(struct_decl.fields, move(param_node));
    }

    advance();
  }

  void parse_declaration (Node &decl_node) {
    auto name_token = *current;
    advance();

    consume(Token::Colon);

    if      (looking_at(Token::Colon)) decl_node.kind = Node::Decl_Constant_Value;
    else if (looking_at(Token::Equal)) decl_node.kind = Node::Decl_Variable_Value;

    advance();

    if (looking_at(Token::Open_Round_Bracket)) {
      decl_node.kind = Node::Decl_Lambda;
      auto &lambda_decl = decl_node.lambda_decl;

      lambda_decl.name   = name_token;
      lambda_decl.params = List<Parameter_Node>(arena);
      lambda_decl.body   = List<Node*>(arena);

      parse_parameter_list(lambda_decl.params);

      if (current != Token::Open_Curly_Bracket) {
        parse_type(lambda_decl.return_type);
      }

      assert(current == Token::Open_Curly_Bracket);

      consume(Token::Open_Curly_Bracket);

      parse_until(Token::Close_Curly_Bracket) {
        auto *code_line = reserve<Node>(arena);
        parse_next(*code_line);

        list_push(lambda_decl.body, move(code_line));
      }

      advance();

      return;
    }

    if (looking_at(Token::Open_Curly_Bracket)) {
      //parse_block();
      return;
    }

    if (looking_at(Token::Struct)) {
      advance();

      decl_node.kind = Node::Decl_Struct;
      auto &struct_decl = decl_node.struct_decl;

      struct_decl.name   = name_token;
      struct_decl.params = List<Parameter_Node>(arena);

      if (looking_at(Token::Open_Round_Bracket)) {
        parse_parameter_list(struct_decl.params);
      }

      parse_struct_body(struct_decl);

      return;
    }

    assert(decl_node.kind == Node::Decl_Constant_Value ||
           decl_node.kind == Node::Decl_Variable_Value);

    decl_node.expr_value = reserve<Node>(arena);
    parse_expression(*decl_node.expr_value);
  }

  void parse_next (Node &node) {
    switch (current->kind) {
      case Token::Symbol: {
        advance();

        if (looking_at(Token::Colon)) {
          advance();

          if      (current == Token::Colon) advance();
          else if (current == Token::Equal) advance();
          else assert(false && "Unexpected token");

          rewind(3);

          parse_declaration(node);

          return;
        }

        rewind();
        parse_expression(node);

        return;
      }
      default: {
        assert(false && "Unsupported token");
      }
    }
  }

};

Node build_tree(Parser &parser) {
  Node tree {};
  
  while (!parser.looking_at(Token::Last))  {
    switch (parser.current->kind) {
      case Token::Use: {
        assert(false && "Support for use statements");
        break;
      }
      case Token::Symbol: {
        Node decl_node {};
        parser.parse_declaration(decl_node);
        break;
      }
      default: {
        assert(false && "Unexpected top-level token");
      }
    }
  }

  return {};
}


int main (int argc, char **argv) {
  auto file = read_file_into_memory("samples/main.eko");

  Memory_Arena arena { reserve_virtual_memory(megabytes(64)) };

  auto tokens_buffer = reserve<Token>(arena, 1024 * sizeof(Token));

  File_Tokenizer tokenizer(arena, file);
  read_tokens(tokenizer, tokens_buffer);
  
  Parser parser(arena, tokens_buffer);
  auto tree = build_tree(parser);
  
  return 0;
}
