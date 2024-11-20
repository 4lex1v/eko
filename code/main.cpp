
#include <cassert>
#include <cstdint>
#include <cstdlib>

#include <filesystem>
#include <fstream>
#include <string>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>

#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>

using u8 = uint8_t;
using u32 = uint32_t;
using usize = size_t;

#define INCOMPLETE() assert(false && "Incomplete")

struct Node;
struct Type_Node;
struct Type;

// struct Node {
//   enum Kind {
//     Undefined,

//     Root,
//     Decl_Value,
//     Decl_Lambda,
//     Decl_Struct,

//     Identifier,
//     Literal,

//     Function_Call,
//     Return
//   };

//   Kind kind = Undefined;
// };

// struct Root_Node: Node {
//   std::vector<Node *> decls;

//   Root_Node(): Node(Root) {}
// };

struct Parser_Error {};

struct Binding;

/*
  TYPER
 */


using Type_Table = std::unordered_map<std::string, Type *>;

struct Basic_Types: Type {
  static constexpr auto bool_type = Type(Basic_Bool);
  static constexpr auto signed_32 = Type(Basic_S32);
  static constexpr auto unsigned_32 = Type(Basic_U32);
  static constexpr auto string_type = Type(Basic_String);
};

struct Struct_Type: Type {
  // TODO: Do we need entire binding, or just having types for the layout is
  // sufficient?
  std::vector<Binding *> fields;

  explicit Struct_Type(): Type(Struct) {}
};

struct Pointer_Type: Type {
  const Type *element_type;

  explicit Pointer_Type(Type *_element_type = nullptr)
     : Type(Pointer), element_type{_element_type} {}
};

// static const Type * get_expression_type (Node * node) {
//   if (node->kind == Node::Literal) {
//     auto lit_node = static_cast<Literal_Node *>(node);

//     if (lit_node->value.kind == Token::False || lit_node->value.kind ==
//     Token::True) {
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
  std::vector<Entry *> entries{};

  explicit Block(): Entry(Entry::Block) {}
};

struct Expression: Entry {
  explicit Expression(): Entry(Entry::Expression) {}
};

struct Binding: Entry {
  enum Kind { Lambda, Variable, Constant };

  Kind kind;
  Token name;

  explicit Binding(Kind _kind, Token _name)
     : Entry(Entry::Binding), kind{_kind}, name{std::move(_name)} {}
};

struct Variable_Binding: Binding {
  const Type *type = nullptr;
 ::Expression *expr = nullptr;

  explicit Variable_Binding(Token name): Binding(Variable, std::move(name)) {}
};

struct Lambda_Binding: Binding {
  std::vector<Binding *> params{};
  const Type *return_type = nullptr;

 ::Block block;

  explicit Lambda_Binding(Token name): Binding(Lambda, std::move(name)) {}
};

static const Type *get_basic_type (const Token &name) {
  if (name.value == "bool")
    return &Basic_Types::bool_type;
  if (name.value == "s32")
    return &Basic_Types::signed_32;
  if (name.value == "u32")
    return &Basic_Types::unsigned_32;
  if (name.value == "String")
    return &Basic_Types::string_type;

  return nullptr;
}

static void add_binding_to_scope (Scope &scope, Binding *value) {
  scope.bindings[value->name.value] = value;
}


static llvm::LLVMContext llvm_context{};

static llvm::Type *to_llvm_type (const Type *type) {
  switch (type->kind) {
    case Type::Basic_Bool:
      return llvm::Type::getInt1Ty(llvm_context);

    case Type::Basic_S32:
    case Type::Basic_U32:
      return llvm::Type::getInt32Ty(llvm_context);

    case Type::Struct: {
      auto st = static_cast<const Struct_Type *>(type);

      std::vector<llvm::Type *> fields;
      for (auto &f: st->fields) {
        assert(f->kind == Binding::Kind::Variable);

        auto binding = static_cast<const Variable_Binding *>(f);
        fields.push_back(to_llvm_type(binding->type));
      }

      return llvm::StructType::create(llvm_context, fields, "Some_Struct");
    }

    default: {
      INCOMPLETE();
      return nullptr;
    }
  }
}

static llvm::Value *to_llvm_value (const Expression *expr) {
  INCOMPLETE();
  return nullptr;
}

int main (int, char **) {
  auto file_content = read_file_into_memory("samples/main.eko");
  if (file_content.buffer_size == 0) {
    printf("Couldn't read file's content into a local buffer.");
    return 1;
  }

  auto tokenizer = Tokenizer{file_content};
  auto parser = Parser{tokenizer};
  auto typer = Typer{};

  auto tree = build_tree(parser);
  typer.process(tree);

  /*
    After building the tree we do all the type magic, inferring types and
    checking that all the code is legit.
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
    Not sure what backend includes, something like optimizations and code
    generation I suppose?
   */
  auto module = llvm::Module("eko", llvm_context);
  module.setDataLayout(target_machine->createDataLayout());
  module.setTargetTriple(target_triple);

  for (const Binding *decl: typer.bindings) {
    if (decl->kind == Binding::Lambda) {
      auto lambda = static_cast<const Lambda_Binding *>(decl);

      auto llvm_ret_type = to_llvm_type(lambda->return_type);

      std::vector<llvm::Type *> param_types;
      for (auto p: lambda->params) {
        auto var = static_cast<const Variable_Binding *>(p);
        assert(var->type);

        param_types.push_back(to_llvm_type(var->type));
      }

      auto function_type =
          llvm::FunctionType::get(llvm_ret_type, param_types, false);

      auto lambda_func = llvm::Function::Create(
          function_type, llvm::GlobalValue::ExternalLinkage, lambda->name.value,
          &module);

      auto entry_block =
          llvm::BasicBlock::Create(llvm_context, "entry", lambda_func);
      auto entry_block_builder = llvm::IRBuilder(entry_block);

      for (auto entry: lambda->block.entries) {
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
              if (var->expr)
                lv_init_value = to_llvm_value(var->expr);

              auto var_slot = entry_block_builder.CreateAlloca(
                  lv_var_type, lv_init_value, var->name.value);

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

  std::string error_message;
  llvm::raw_string_ostream err_stream(error_message);

  

  // if (verifyFunction(*func, &err_stream)) {
  //   err_stream.flush();
  //   llvm::errs() << "Function verification failed: " << error_message <<
  //   "\n"; exit(1);
  // }
  //

  if (llvm::verifyModule(module, &err_stream)) {
    err_stream.flush();
    llvm::errs() << "Module verification failed: " << error_message << "\n";
    exit(1);
  }

  module.print(llvm::outs(), nullptr);

  return 0;
}
