
#include <cassert>
#include <cstdlib>
#include <cstdint>

#include <string>
#include <filesystem>
#include <fstream>

#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>

#include <llvm/Target/TargetOptions.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>

using u8    = uint8_t;
using u32   = uint32_t;
using usize = size_t;

#define NOT_COMPLETE assert(false && "Incomplete")

struct Loaded_File {
  std::string file_name;
  std::string extension;

  const char *buffer;
  usize       buffer_size;
};

static Loaded_File read_file_into_memory (const std::filesystem::path &file_path) {
  std::ifstream file_handle { file_path, std::ios::binary | std::ios::ate };
  if (file_handle.is_open() == false) return {};

  auto file_size = static_cast<usize>(file_handle.tellg());
  if (file_size <= 1) return {};
  file_handle.seekg(std::ios::beg);

  Loaded_File file {
    .file_name   = file_path.filename().string(),
    .extension   = file_path.extension().string(),

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
    False,
    True,

    Newline,
    Numeric, // This should be refined into different types of literals supported, integers and floats at least
    String_Literal,
    Symbol,
  };

  Kind kind = Undefined;
  std::string value;
  u32 row = 0;
  u32 column = 0;

  bool operator == (Kind kind)                const { return this->kind  == kind; }
  bool operator == (const std::string_view &value) const { return this->value == value; }
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
    tokens.reserve(64);

    while (true) {
      auto token   = read_token();
      auto is_last = token.kind == Token::Last;
      tokens.push_back(token);

      if (is_last) break;
    }
  }

  /*
    Tokenizer Functions
  */

  [[nodiscard]] Token make_token (Token::Kind kind, std::string value = {}) const {
    return Token(kind, std::move(value), row, column);
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

  [[nodiscard]] bool looking_at (u8 value) const { return *current == value; }

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

    auto value = std::string(value_start, current - value_start);
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

    auto token_value = std::string(value_start, current - value_start);
    return make_token(Token::String_Literal, token_value);
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
  PARSER
*/

#include <deque>

struct Node;
struct Type_Node;
struct Type;

static std::deque<Node *> checker_queue;

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
      type_name  { std::move(name) },
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
    Return
  };

  Kind kind = Undefined;
};

struct Root_Node: Node {
  std::vector<Node *> decls;

  Root_Node ()
    : Node(Root)
  {}
};

struct Literal_Node: Node {
  Token value;

  explicit Literal_Node (Token _value)
    : Node(Literal),
      value { std::move(_value) }
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
  Token  name;
  Node  *value_expression;

  explicit Value_Decl_Node (Token _name, Node *expr = nullptr, bool is_const = false)
    : Node(Decl_Value),
      name             { std::move(_name) },
      value_expression { expr }
  {}
};

struct Struct_Decl_Node: Node {
  Token name;

  std::vector<Parameter_Node> params;
  std::vector<Parameter_Node> fields;

  explicit Struct_Decl_Node (Token _name)
    : Node(Decl_Struct),
      name { std::move(_name) }
  {}
};

struct Lambda_Decl_Node;

struct Lambda_Decl_Node: Node {
  Token name;

  std::vector<Parameter_Node>  params;
  Type_Node                   *return_type;
  std::vector<Node *>          body;

  explicit Lambda_Decl_Node (Token _name)
    : Node(Decl_Lambda),
      name { std::move(_name) }
  {}
};

struct Function_Call_Node: Node {
  Node *expr               = nullptr;
  std::vector<Node *> args = {};

  Function_Call_Node (): Node(Function_Call) {}
};

struct Return_Node: Node {
  Node *expr = nullptr;

  explicit Return_Node(Node *_expr)
    : Node(Return),
      expr { _expr }
  {}
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
    if (looking_at(Token::String_Literal) || looking_at(Token::Numeric)) {
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

    if (!(looking_at(Token::Colon) || looking_at(Token::Equal))) {
      // TODO: Parsing error
        exit(1);
    }

    const auto is_constant = looking_at(Token::Colon);

    advance();

    if (looking_at(Token::Open_Round_Bracket)) {
        if (!is_constant) {
            // TODO: Handle error, lambda value can't be used as a value, at least for now
            exit(1);
        }

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

    auto declaration_value = parse_expression();
    auto declaration_node = new Value_Decl_Node(name_token, declaration_value, is_constant);

    return declaration_node;
  }

  Node * parse_next () {
    if (looking_at(Token::Return)) {
      advance();

      auto return_expr = parse_expression();

      return new Return_Node(return_expr);
    }

    if (looking_at(Token::Symbol)) {
      advance();

      if (looking_at(Token::Colon)) {
        advance();

        if (!(looking_at(Token::Colon) || looking_at(Token::Equal))) {
          assert(false && "Unexpected token");
          exit(1);
        }

        rewind(2);

        return parse_declaration();
      }

      rewind();
      return parse_expression();
    }

    assert(false && "Unsupported token");
    return nullptr;
  }

};

Root_Node build_tree (Parser &parser) {
  Root_Node root;

  while (true)  {
    if (parser.looking_at(Token::Use)) {
      assert(false && "Support for use statements");
      break;
    }

    if (parser.looking_at(Token::Symbol)) {
      root.decls.push_back(parser.parse_declaration());
      continue;
    }

    if (parser.looking_at(Token::Last)) break;

    assert(false && "Unsupported token");
  }

  return root;
}

struct Binding;

/*
  TYPER
 */

struct Type {
  enum Kind {
    Invalid,

    Basic_Bool,
    Basic_S32,
    Basic_String,

    Struct,
    Pointer,
  };

  Kind kind = Invalid;

  explicit constexpr Type (Kind k): kind { k } {}
};

struct Basic_Types: Type {
  static constexpr auto bool_type   = Type(Basic_Bool);
  static constexpr auto signed_32   = Type(Basic_S32);
  static constexpr auto string_type = Type(Basic_String);
};

struct Struct_Type: Type {

};

struct Pointer_Type: Type {
  Type *element_type;

  explicit Pointer_Type (Type *_element_type = nullptr)
    : Type (Pointer),
      element_type { _element_type }
  {}
};

// static const Type * get_expression_type (Node * node) {
//   if (node->kind == Node::Literal) {
//     auto lit_node = static_cast<Literal_Node *>(node);

//     if (lit_node->value.kind == Token::False || lit_node->value.kind == Token::True) {
//       return &Basic_Types::bool_type;
//     }

//     if (lit_node->value.kind == Token::String_Literal) {
//       return &Basic_Types::string_type;
//     }

//   }

//   assert(false && "Incomplete");

//   return nullptr;
// }

struct Scope {
  Scope *parent = nullptr;

  std::unordered_map<std::string_view, Binding *> bindings;
};

struct Entry {
  enum Kind {
    Block,
    Binding,
    Expression,
    Control,
  };

  Kind kind;
};

struct Block: Entry {
  std::vector<Entry *> entries;

  explicit Block ()
    : Entry(Entry::Block)
  {}
};

struct Expression: Entry {
  explicit Expression ()
    : Entry(Entry::Expression)
  {}
};

struct Binding: Entry {
  enum Kind {
    Lambda,
    Variable,
    Constant
  };

  Kind kind;
  Token name;

  explicit Binding (Kind _kind, Token _name)
    : Entry(Entry::Binding),
      kind { _kind },
      name { std::move(_name) }
  {}
};

struct Variable_Binding: Binding {
  const Type *type = nullptr;
  ::Expression *expr = nullptr;
  
  explicit Variable_Binding (Token name)
    : Binding(Variable, std::move(name))
  {}
};

struct Lambda_Binding: Binding {
  std::vector<Binding *> params {};
  const Type *return_type = nullptr;
  ::Block *block          = nullptr;

  explicit Lambda_Binding (Token name)
    : Binding(Lambda, std::move(name))
  {}
};

static const Type * get_basic_type (const Token &name) {
  if (name.value == "bool")   return &Basic_Types::bool_type;
  if (name.value == "s32")    return &Basic_Types::signed_32;

  return nullptr;
}

static const Type * typecheck_type (Type_Node *type) {
  switch (type->kind) {
    case Type_Node::Pointer: {
      auto pt = static_cast<Pointer_Type_Node *>(type);
      return new Pointer_Type(nullptr);
    }
    case Type_Node::Plain: {
      auto pt = static_cast<Plain_Type_Node *>(type);
      if (pt->parameters.empty() == false) {
        assert(false && "Unsupported");
        return nullptr;
      }

      auto basic_type = get_basic_type(pt->type_name);
      if (basic_type) return basic_type;

      assert(false && "Unsupported");
      
      return nullptr;
    }
    default: {
      assert(false && "Unsupported");
      return nullptr;
    }
  }
}

static void add_binding_to_scope (Scope &scope, Binding *value) {
  scope.bindings[value->name.value] = value;
}

static Binding * typecheck_lambda_binding (const Lambda_Decl_Node *lambda_decl) {
  auto lambda = new Lambda_Binding(lambda_decl->name);

  /*
    Handle parameters
   */
  for (auto &param: lambda_decl->params) {
    auto param_binding = new Variable_Binding(param.name);
    param_binding->type = typecheck_type(param.type);

    lambda->params.push_back(param_binding);
  }
  
  lambda->return_type = typecheck_type(lambda_decl->return_type);

  return lambda;
}

static std::vector<Binding *> process (Root_Node &root) {
  std::vector<Binding *> result;
  
  for (auto decl: root.decls) {
    if (decl->kind == Node::Decl_Lambda) {
      auto lambda_decl = static_cast<Lambda_Decl_Node *>(decl);
      result.push_back(typecheck_lambda_binding(lambda_decl));
    }
  }

  return result;
}

static llvm::LLVMContext llvm_context {};

static llvm::Type * to_llvm_type (const Type *type) {
  switch (type->kind) {
    case Type::Basic_Bool: return llvm::Type::getInt1Ty(llvm_context);
    case Type::Basic_S32:  return llvm::Type::getInt32Ty(llvm_context);
    default: {
      NOT_COMPLETE;
      return nullptr;
    }
  }
}

static llvm::Value * to_llvm_value (const Expression *expr) {
  NOT_COMPLETE;
  return nullptr;
}

int main (int, char **) {
  auto file_content = read_file_into_memory("samples/main.eko");
  if (file_content.buffer_size == 0) {
    printf("Couldn't read file's content into a local buffer.");
    return 1;
  }

  auto tokenizer = Tokenizer { file_content };
  auto parser    = Parser    { tokenizer };

  auto tree = build_tree(parser);

  auto decls = process(tree);

  /*
    After building the tree we do all the type magic, inferring types and checking that all the code is legit.
   */
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmParser();
  llvm::InitializeNativeTargetAsmPrinter();

  const auto target_triple = llvm::sys::getDefaultTargetTriple();
  llvm::outs() << "Target triple: " << target_triple << "\n";

  std::string lookup_error;
  const auto target = llvm::TargetRegistry::lookupTarget(target_triple, lookup_error);
  if (!target) {
    llvm::errs() << "Couldn't create LLVM target: " << lookup_error << "\n";
    exit(1);
  }

  auto target_machine = target->createTargetMachine(target_triple, "x86-64", "", {}, {});

  /*
    Not sure what backend includes, something like optimizations and code generation I suppose?
   */
  auto module = llvm::Module("eko", llvm_context);
  module.setDataLayout(target_machine->createDataLayout());
  module.setTargetTriple(target_triple);

  for (const Binding *decl : decls) {
    if (decl->kind == Binding::Lambda) {
      auto lambda = static_cast<const Lambda_Binding *>(decl);

      auto llvm_ret_type = to_llvm_type(lambda->return_type);

      std::vector<llvm::Type *> param_types;
      for (auto p: lambda->params) {
        auto var = static_cast<const Variable_Binding *>(p);
        assert(var->type);

        param_types.push_back(to_llvm_type(var->type));
      }

      auto function_type = llvm::FunctionType::get(llvm_ret_type, param_types, false);

      auto lambda_func = llvm::Function::Create(function_type, llvm::GlobalValue::ExternalLinkage, lambda->name.value, &module);

      auto entry_block         = llvm::BasicBlock::Create(llvm_context, "entry", lambda_func);
      auto entry_block_builder = llvm::IRBuilder(entry_block);

      for (auto entry: lambda->block->entries) {
        if (entry->kind == Entry::Binding) {
            switch (static_cast<Binding *>(entry)->kind) {
                case Binding::Constant: {
                    break;
                }
                case Binding::Variable: {
                    auto var = static_cast<Variable_Binding *>(entry);

                    assert(var->type);

                    auto lv_var_type = to_llvm_type(var->type);

                    llvm::Value *lv_init_value = nullptr;
                    if (var->expr) lv_init_value = to_llvm_value(var->expr);

                    auto var_slot = entry_block_builder.CreateAlloca(lv_var_type, lv_init_value, var->name.value);

                    break;
                }
                default: {
                    assert(false && "Unsupported");
                    break;
                }
            }
        }
      }
      
    }
  }

  // auto void_type = llvm::Type::getVoidTy(llvm_context);
  //
  // auto func_type = llvm::FunctionType::get(void_type, {}, false);
  // auto func      = llvm::Function::Create(func_type, llvm::GlobalValue::PrivateLinkage, "main", &module);
  //
  // auto entry_block         = llvm::BasicBlock::Create(llvm_context, "entry", func);
  // auto entry_block_builder = llvm::IRBuilder(entry_block);
  // entry_block_builder.CreateRetVoid();

  // std::string error_message;
  // llvm::raw_string_ostream err_stream(error_message);
  // if (verifyFunction(*func, &err_stream)) {
  //   err_stream.flush();
  //   llvm::errs() << "Function verification failed: " << error_message << "\n";
  //   exit(1);
  // }
  //
  // if (llvm::verifyModule(module, &err_stream)) {
  //   err_stream.flush();
  //   llvm::errs() << "Module verification failed: " << error_message << "\n";
  //   exit(1);
  // }

  module.print(llvm::outs(), nullptr);

  return 0;
}
