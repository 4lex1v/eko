
#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Support.h>

#include "eko.hpp"
#include "ast.hpp"
#include "codegen.hpp"

static LLVMContextRef llvm_context;
static const char     *target_triple;

static void init_llvm_generator() {
  LLVMInitializeNativeTarget();
  LLVMInitializeNativeAsmParser();
  LLVMInitializeNativeAsmPrinter();

  target_triple = LLVMGetDefaultTargetTriple();
  log("Target triple: %", target_triple);

  llvm_context = LLVMContextCreate();
}

void codegen (const Root_Node &tree) {
  init_llvm_generator();
  
  auto module = LLVMModuleCreateWithNameInContext("eko", llvm_context);
  LLVMSetTarget(module, target_triple);
  
  LLVMContextDispose(llvm_context);
}
