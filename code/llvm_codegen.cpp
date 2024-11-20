
#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Support.h>
#include <llvm-c/Analysis.h>

#include "anyfin/memory.hpp"
#include "anyfin/prelude.hpp"

#include "eko.hpp"
#include "ast.hpp"
#include "codegen.hpp"

namespace Eko {

static LLVMContextRef llvm_context;
static const char     *target_triple;
static LLVMTargetMachineRef target_machine;

static void init_llvm_generator() {
  LLVMInitializeNativeTarget();
  LLVMInitializeNativeAsmParser();
  LLVMInitializeNativeAsmPrinter();

  target_triple = LLVMGetDefaultTargetTriple();

  char *error = NULL;
  LLVMTargetRef target;
  if (LLVMGetTargetFromTriple(target_triple, &target, &error)) {
    log("Error: %s\n", error);
    LLVMDisposeMessage(error);
    return;
  }

  auto opt_level  = LLVMCodeGenLevelDefault; 
  auto reloc_mode = LLVMRelocDefault; 
  auto code_model = LLVMCodeModelDefault; 

  target_machine = LLVMCreateTargetMachine(target, target_triple, "x86-64", "", opt_level, reloc_mode, code_model);

  llvm_context = LLVMContextCreate();
}

struct Binding;

enum struct Binding_Kind {
  
};

enum struct Type_Kind {
  Invalid,

  Basic_Bool,
  Basic_S32,
  Basic_U32,

  Struct,
  Pointer,
};

struct Type {
  using enum Type_Kind;
  
  Type_Kind kind = Invalid;
  String name; // valid for struct types only
};

struct Struct_Binding {
  String name;
  List<Type *> fields;
};

struct Lambda_Binding {
  String name;

  const Type *return_type;
};

static LLVMTypeRef get_llvm_type (LLVMModuleRef module, const Type *type) {
  switch (type->kind) {
    case Type::Basic_Bool: return LLVMInt1TypeInContext(llvm_context);
    case Type::Basic_S32:
    case Type::Basic_U32: return LLVMInt32TypeInContext(llvm_context);
    case Type::Struct:    return LLVMGetTypeByName(module, type->name.value);
    case Type::Pointer:   return nullptr;
    case Type::Invalid: {
      fin_ensure(false && "Invalid type"); 
      return nullptr;
    }
  }
}

static void generate_function_ir (LLVMModuleRef unit, const Lambda_Binding &value) {
  auto return_type = get_llvm_type(unit, value.return_type);

  LLVMTypeRef params [] {};

  auto function_type = LLVMFunctionType(return_type, params, 0, false);
  auto function      = LLVMAddFunction(unit, value.name.value, function_type);
}

void codegen () {
  init_llvm_generator();

  auto arena = Memory_Arena(reserve_virtual_memory(megabytes(1)));
  
  auto data_layout     = LLVMCreateTargetDataLayout(target_machine);
  auto data_layout_str = LLVMCopyStringRepOfTargetData(data_layout);

  auto unit = LLVMModuleCreateWithNameInContext("eko", llvm_context);
  LLVMSetTarget(unit, target_triple);
  LLVMSetDataLayout(unit, data_layout_str);

  Struct_Binding value;
  value.name = "Args";

  auto fields = new (arena) LLVMTypeRef[value.fields.count];
  for (usize i = 0; auto &f: value.fields) {
    fields[i++] = get_llvm_type(unit, f);
  }

  auto new_struct = LLVMStructCreateNamed(llvm_context, value.name.value);
  LLVMStructSetBody(new_struct, fields, value.fields.count, false);

  char *error_message = nullptr;
  if (LLVMVerifyModule(unit, LLVMReturnStatusAction, &error_message)) {
    log("Module verification failed: %\n", error_message);
    LLVMDisposeMessage(error_message);
  }
  else {
    auto ir_string = LLVMPrintModuleToString(unit);
    log("%\n", ir_string);
    LLVMDisposeMessage(ir_string);
  }

  LLVMDisposeMessage(data_layout_str);
  LLVMDisposeTargetData(data_layout);
  LLVMContextDispose(llvm_context);
}

}
