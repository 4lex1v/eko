
#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Support.h>
#include <llvm-c/Analysis.h>

#include "anyfin/memory.hpp"
#include "anyfin/prelude.hpp"

#include "eko.hpp"
#include "ast.hpp"
#include "typer.hpp"
#include "codegen.hpp"

using namespace Fin;

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
    LLVMDisposeMessage(error);
    return;
  }

  auto opt_level  = LLVMCodeGenLevelDefault; 
  auto reloc_mode = LLVMRelocDefault; 
  auto code_model = LLVMCodeModelDefault; 

  target_machine = LLVMCreateTargetMachine(target, target_triple, "x86-64", "", opt_level, reloc_mode, code_model);

  llvm_context = LLVMContextCreate();
}

// static LLVMTypeRef get_llvm_type (LLVMModuleRef module, const Type *type) {
// }

static void generate_function_ir (LLVMModuleRef unit, const Lambda_Binding &value) {
  // auto return_type = get_llvm_type(unit, value.return_type);

  // LLVMTypeRef params [] {};

  // auto function_type = LLVMFunctionType(return_type, params, 0, false);
  // auto function      = LLVMAddFunction(unit, value.name.value, function_type);
}

void codegen (const Source_File &file) {
  init_llvm_generator();

  auto arena = Memory_Arena(reserve_virtual_memory(megabytes(1)));
  
  auto data_layout     = LLVMCreateTargetDataLayout(target_machine);
  auto data_layout_str = LLVMCopyStringRepOfTargetData(data_layout);

  auto unit = LLVMModuleCreateWithNameInContext("eko", llvm_context);
  LLVMSetTarget(unit, target_triple);
  LLVMSetDataLayout(unit, data_layout_str);

  for (auto &decl: file.top_level) {
    if (decl->kind == Binding::Lambda) {
      generate_function_ir(unit, decl->lambda_binding);
    }
  }

  char *error_message = nullptr;
  if (LLVMVerifyModule(unit, LLVMReturnStatusAction, &error_message)) {
    LLVMDisposeMessage(error_message);
  }
  else {
    auto ir_string = LLVMPrintModuleToString(unit);
    LLVMDisposeMessage(ir_string);
  }

  LLVMDisposeMessage(data_layout_str);
  LLVMDisposeTargetData(data_layout);
  LLVMContextDispose(llvm_context);
}
