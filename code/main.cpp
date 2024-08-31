
#include <cassert>
#include <cstdlib>
#include <cstdint>

#include <string>
#include <print>
#include <filesystem>
#include <fstream>

using u8    = uint8_t;
using u32   = uint32_t;
using usize = size_t;

struct Loaded_File {
  const char *buffer;
  usize       buffer_size;
};

Loaded_File read_file_into_memory (const std::filesystem::path &file_path) {
  std::ifstream file_handle { file_path, std::ios::binary | std::ios::ate };
  if (file_handle.is_open() == false) return {};

  auto file_size = static_cast<usize>(file_handle.tellg());
  if (file_size <= 1) return {};
  file_handle.seekg(std::ios::beg);

  Loaded_File file {
    .buffer      = static_cast<char*>(calloc(file_size, sizeof(char))),
    .buffer_size = file_size
  };

  if (!file_handle.read(const_cast<char *>(file.buffer), file_size)) return {};

  return file;
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
  std::string_view value;
  u32 row = 0;
  u32 column = 0;

  bool operator == (Kind kind)                const { return this->kind  == kind; }
  bool operator == (const std::string &value) const { return this->value == value; }
};

static bool operator == (const Token *token, Token::Kind kind) { return token->kind == kind; }

static bool is_whitespace (char value) {
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

struct Tokenizer {
  const char *buffer;
  const char *current;

  std::vector<Token> tokens;

  u32 row    = 0;
  u32 column = 0;

  explicit Tokenizer (Loaded_File file)
    : buffer  { file.buffer },
      current { buffer }
  {
    /*
      TODO: It would be nice to get a better heuristic for how big the size of this vector should be.
            Perhaps it could be based on the size of the input file?
     */
    tokens.resize(64);

    while (true) {
      auto token   = read_token();
      auto is_last = token.kind == Token::Last;
      tokens.push_back(std::move(token));

      if (is_last) break;
    }
  }

  /*
    Tokenizer Functions
  */

  Token make_token (Token::Kind kind, std::string_view value = {}) const {
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

  bool looking_at (u8 value) const { return *current == value; }

  void skip_basic_whitespace () {
    while (*current == ' ' || *current == '\t') current += 1;
  }

  Token read_symbolic_token () {
    assert(looking_at('_') || is_alpha(*current));

    const char *value_start = current;
    while (true) {
      if (*current == '_' || is_alpha(*current) || is_numeric(*current)) {
        advance();
        continue;
      }

      break;
    }

    auto value = std::string_view(value_start, current - value_start);
    if (value == "return") return make_token(Token::Return);
    if (value == "struct") return make_token(Token::Struct);
    if (value == "use")    return make_token(Token::Use);

    return make_token(Token::Symbol, value);
  }

  Token read_numeric_token () {
    assert(is_numeric(*current));

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

    return make_token(Token::Numeric, std::string(value_start, current - value_start));
  }

  Token read_string_literal () {
    assert(looking_at('"'));

    auto value_start = current;
    if (advance()) { // skip the opening " of a string literal
      assert(false && "Handle error with unclosed string literal");
      return {};
    }

    /*
      TODO: This need a proper function that handles a string literal with all the escapes and other stuff
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
      At this point the tokenizer is one symbol after the string literal closing quote,
      so no additional advances are needed.
    */

    return make_token(Token::String_Literal, std::string(value_start, current - value_start));
  }

  Token read_token () {
    skip_basic_whitespace();

    if (looking_at('\0')) return make_token(Token::Last);

    if (looking_at('\r')) {
      advance();
      assert(looking_at('\n'));
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
      case ':': result = make_token(Token::Colon,                ":"); break;
      case '(': result = make_token(Token::Open_Round_Bracket,   "("); break;
      case ')': result = make_token(Token::Close_Round_Bracket,  ")"); break;
      case '[': result = make_token(Token::Open_Square_Bracket,  "["); break;
      case ']': result = make_token(Token::Close_Square_Bracket, "]"); break;
      case '{': result = make_token(Token::Open_Curly_Bracket,   "{"); break;
      case '}': result = make_token(Token::Close_Curly_Bracket,  "}"); break;
      case ',': result = make_token(Token::Coma,                 ","); break;
      case ';': result = make_token(Token::Semicolon,            ";"); break;
      case '=': result = make_token(Token::Equal,                "="); break;
      case '.': result = make_token(Token::Period,               "."); break;
      case '+': result = make_token(Token::Plus,                 "+"); break;
      case '-': result = make_token(Token::Minus,                "-"); break;
      case '*': result = make_token(Token::Star,                 "*"); break;
      case '_': result = make_token(Token::Underscore,           "_"); break;

      default: assert(false && "Unsupported character symbol");
    }

    advance();

    return result;
  }
};

/*
  Parser
*/

struct Node;
struct Type_Node;

struct Type_Node {
  enum Kind {
    Pointer,
    Array,
    Seq,
    Plain
  };

  Kind kind;
};

struct Pointer_Type_Node: Type_Node {
  Type_Node *value_type;

  Pointer_Type_Node (): Pointer_Type_Node(nullptr) {}

  explicit Pointer_Type_Node (Type_Node *_value_type)
    : Type_Node(Pointer),
      value_type { _value_type }
  {}
};

struct Array_Type_Node: Type_Node {
  Node      *bounds_expression;
  Type_Node *elements_type;

  explicit Array_Type_Node (Node *expr = nullptr, Type_Node *_elements_type = nullptr)
    : Type_Node(Seq),
      bounds_expression { expr },
      elements_type     { _elements_type }
  {}
};

struct Seq_Type_Node: Type_Node {
  Type_Node *element_type;

  explicit Seq_Type_Node (Type_Node *_element_type = nullptr)
    : Type_Node(Seq),
      element_type { _element_type }
  {}
};

struct Plain_Type_Node: Type_Node {
  Token type_name;
  std::vector<Type_Node*> parameters;

  explicit Plain_Type_Node (Token name, std::vector<Type_Node *> params = {})
    : Type_Node(Plain),
      type_name  { name },
      parameters { std::move(params) }
  {}
};


struct Parameter_Node {
  Token      name;
  Type_Node *type = nullptr;
  Node      *default_init = nullptr;
};

struct Node {
  enum Kind {
    Undefined,

    Root,
    Decl_Value,
    Decl_Lambda,
    Decl_Struct,

    Identifier,
    Literal,

    Function_Call,
  };

  Kind kind = Undefined;
};

struct Literal_Node: Node {
  Token value;

  explicit Literal_Node (Token _value)
    : Node(Literal),
      value { _value }
  {}
};

struct Identifier_Node: Node {
  /*
    Identifier expression could be however long, consisting of multiple parts
   */
  std::vector<Token> identifier;

  explicit Identifier_Node (std::vector<Token> ids = {})
    : Node(Identifier),
      identifier { std::move(ids) }
  {}
};

struct Value_Decl_Node: Node {
  Node *value_expression;
  bool is_const;

  Value_Decl_Node (Node *expr = nullptr, bool _is_const = false)
    : Node(Decl_Value),
      value_expression { expr },
      is_const         { _is_const }
  {}
};

struct Struct_Decl_Node: Node {
  Token name;

  std::vector<Parameter_Node> params;
  std::vector<Parameter_Node> fields;

  explicit Struct_Decl_Node (Token _name)
    : Node(Decl_Struct),
      name { _name }
  {}
};

struct Lambda_Decl_Node: Node {
  Token name;

  std::vector<Parameter_Node>  params;
  Type_Node                   *return_type;
  std::vector<Node *>          body;

  explicit Lambda_Decl_Node (Token _name)
    : Node(Decl_Lambda),
      name { _name }
  {}
};

struct Function_Call_Node: Node {
  Node *expr               = nullptr;
  std::vector<Node *> args = {};

  Function_Call_Node (): Node(Function_Call) {}
};

struct Parser_Error {};

struct Parser {
  const Tokenizer  tokenizer;
  const Token     *current;

  explicit Parser (Tokenizer _tokenizer)
    : tokenizer { std::move(_tokenizer) },
      current   { tokenizer.tokens.data() }
  {}

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

  Type_Node * parse_type () {
    if (looking_at(Token::Star)) {
      advance();
      return new Pointer_Type_Node(parse_type());
    }

    if (looking_at(Token::Open_Square_Bracket)) {
      advance();

      if (looking_at(Token::Close_Square_Bracket)) {
        advance();
        return new Seq_Type_Node(parse_type());
      }

      auto bounds_argument = parse_expression();
      consume(Token::Close_Square_Bracket);
      auto elem_type = parse_type();

      return new Array_Type_Node(bounds_argument, elem_type);
    }

    if (looking_at(Token::Symbol)) {
      auto plain_type = new Plain_Type_Node(*current);
      advance();

      if (looking_at(Token::Open_Round_Bracket)) {
        advance();

        parse_until(Token::Close_Round_Bracket) {
          plain_type->parameters.push_back(parse_type());
          if (looking_at(Token::Coma)) advance();
        }

        advance();
      }

      return plain_type;
    }

    assert(false && "Unexpected symbol in the type");

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

  std::vector<Parameter_Node> parse_parameter_list () {
    consume(Token::Open_Round_Bracket);

    std::vector<Parameter_Node> params; // TODO: can count the number of comas to size the vector correctly?

    parse_until(Token::Close_Round_Bracket) {
      params.push_back(parse_parameter());
      if (looking_at(Token::Coma)) advance();
    }

    advance(); // past the closing bracket

    return params;
  }

  Node * parse_unary_expression () {
    if (looking_at(Token::String_Literal)) {
      auto node = new Literal_Node(*current);
      advance();
      return node;
    }

    if (looking_at(Token::Numeric)) {
      auto node = new Literal_Node(*current);
      advance();
      return node;
    }

    if (looking_at(Token::Symbol)) {
      auto id_list = std::vector<Token>();
      id_list.resize(8);

      id_list.push_back(*current);
      advance();

      if (looking_at(Token::Period)) {
        advance();
        while (true) {
          skip_new_line_tokens();
          assert(current == Token::Symbol);

          id_list.push_back(*current);
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

        auto call_node = new Function_Call_Node;
        // TODO: There are other ways to get a function expr...
        call_node->expr = new Identifier_Node(std::move(id_list));

        if (!looking_at(Token::Close_Round_Bracket))
          call_node->args.resize(8); // TODO: Perhaps this could also be calculated with a better precision?

        parse_until(Token::Close_Round_Bracket) {
          call_node->args.push_back(parse_expression());
          if (looking_at(Token::Coma)) advance();
        }

        consume(Token::Close_Round_Bracket);

        return call_node;
      }

      return new Identifier_Node(id_list);
    }

    assert(false && "Unsupported expression kind");
    return nullptr;
  }

  Node * parse_expression () {
    return parse_unary_expression();
  }

  Node * parse_declaration () {
    auto name_token = *current;
    advance();

    consume(Token::Colon);

    if (looking_at(Token::Colon) || looking_at(Token::Equal)) {
      advance();
      return new Value_Decl_Node(parse_expression(), looking_at(Token::Colon));
    }

    advance();

    if (looking_at(Token::Open_Round_Bracket)) {
      auto lambda_decl = new Lambda_Decl_Node(name_token);
      lambda_decl->params = parse_parameter_list();

      if (!looking_at(Token::Open_Curly_Bracket)) {
        lambda_decl->return_type = parse_type();
      }

      assert(current == Token::Open_Curly_Bracket);
      consume(Token::Open_Curly_Bracket);

      parse_until(Token::Close_Curly_Bracket) {
        lambda_decl->body.push_back(parse_next());
      }

      advance();

      return lambda_decl;
    }

    if (looking_at(Token::Struct)) {
      advance();

      auto struct_decl = new Struct_Decl_Node(name_token);

      if (looking_at(Token::Open_Round_Bracket)) {
        struct_decl->params = parse_parameter_list();
      }

      consume(Token::Open_Curly_Bracket);

      if (!looking_at(Token::Close_Curly_Bracket))
        struct_decl->fields.resize(8);

      parse_until(Token::Close_Curly_Bracket) {
        struct_decl->fields.push_back(parse_parameter());
      }

      consume(Token::Close_Curly_Bracket);

      return struct_decl;
    }

    assert(false && "Must not be reachable");

    return nullptr;
  }

  Node * parse_next () {
    if (looking_at(Token::Symbol)) {
      advance();

      if (looking_at(Token::Colon)) {
        advance();

        if      (looking_at(Token::Colon)) advance();
        else if (looking_at(Token::Equal)) advance();
        else assert(false && "Unexpected token");

        rewind(3);

        return parse_declaration();
      }

      rewind();
      return parse_expression();
    }

    assert(false && "Unsupported token");
    return nullptr;
  }

};

Node * build_tree(Parser &parser) {
  while (!parser.looking_at(Token::Last))  {
    if (parser.looking_at(Token::Use)) {
      assert(false && "Support for use statements");
      break;
    }

    if (parser.looking_at(Token::Symbol))
      return parser.parse_declaration();

    assert(false && "Unsupported token");
  }

  return {};
}

#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>

#include <llvm/TargetParser/Host.h>
#include <llvm/MC/TargetRegistry.h>

int main (int argc, char **argv) {
  auto file_content = read_file_into_memory("samples/main.eko");
  if (file_content.buffer_size == 0) {
    printf("Couldn't read file's content into a local buffer.");
    return 1;
  }

  auto tokenizer = Tokenizer { std::move(file_content) };
  auto parser    = Parser    { tokenizer };

  auto tree = build_tree(parser);

  /*
    After building the tree we do all the type magic, inferring types and checking that all the code is legit.
   */

  const auto target_triple = llvm::sys::getDefaultTargetTriple();

  std::string lookup_error;
  const auto target = llvm::TargetRegistry::lookupTarget(target_triple, lookup_error);
  if (!target) {
    llvm::errs() << "Couldn't create LLVM target: " << lookup_error << "\n";
    exit(1);
  }

  auto target_machine = target->createTargetMachine(target_triple, "x86-64", "", {}, )


  /*
    Not sure what backend includes, something like optimizations and code generation I suppose?
   */
  auto context = llvm::LLVMContext();

  auto module = llvm::Module("eko", context);
  module.setTargetTriple(llvm::sys::getDefaultTargetTriple());

  auto void_type = llvm::Type::getVoidTy(context);

  auto func_type = llvm::FunctionType::get(void_type, {}, false);
  auto func      = llvm::Function::Create(func_type, llvm::GlobalValue::PrivateLinkage, "main", &module);

  auto entry_block = llvm::BasicBlock::Create(context, "entry", func);

  auto entry_block_builder = llvm::IRBuilder(entry_block);

  if (verifyFunction(*func)) {
    llvm::errs() << "Function verification failed\n";
    exit(1);
  }

  module.print(llvm::outs(), nullptr);

  return 0;
}
