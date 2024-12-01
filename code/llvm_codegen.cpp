
#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Support.h>
#include <llvm-c/Analysis.h>

#include "anyfin/memory.hpp"
#include "anyfin/prelude.hpp"

#include "eko.hpp"
#include "ast.hpp"
#include "brain.hpp"
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

static LLVMTypeRef get_llvm_type (LLVMModuleRef module, const Type &type) {
  switch (type.kind) {
    case Type::Built_In: {
      switch (type.built_in_type) {
        case Built_In_Type::Void:                 return LLVMVoidType();
        case Built_In_Type::Bool:                 return LLVMInt1Type();
        case Built_In_Type::Signed_Byte:          return LLVMInt8Type();
        case Built_In_Type::Unsigned_Byte:        return LLVMInt8Type();
        case Built_In_Type::Signed_Half_Word:     return LLVMInt16Type();
        case Built_In_Type::Unsigned_Half_Word:   return LLVMInt16Type();
        case Built_In_Type::Signed_Word:          return LLVMInt32Type();
        case Built_In_Type::Unsigned_Word:        return LLVMInt32Type();
        case Built_In_Type::Signed_Double_Word:   return LLVMInt64Type();
        case Built_In_Type::Unsigned_Double_Word: return LLVMInt64Type();
        case Built_In_Type::Float:                return LLVMFloatType();
        case Built_In_Type::Double:               return LLVMDoubleType();
        case Built_In_Type::String_Literal:       return LLVMPointerType(LLVMInt8Type(), 0);
      }
      break;
    }
    case Type::Struct: {
      break;
    }
    case Type::Pointer: {
      auto element_type = get_llvm_type(module, *type.element);
      return LLVMPointerType(element_type, 0);
    }
    case Type::Array: {
      auto element_type = get_llvm_type(module, *type.element);
      return LLVMPointerType(element_type, 0);
    }
    case Type::Seq: {
      auto element_type = get_llvm_type(module, *type.element);
      return LLVMPointerType(element_type, 0);
    }
  }

  return nullptr;
}

Fin::Result<Codegen_Error, void> codegen (Fin::Memory_Arena &arena, const Source_File &file) {
  init_llvm_generator();
  
  auto data_layout     = LLVMCreateTargetDataLayout(target_machine);
  auto data_layout_str = LLVMCopyStringRepOfTargetData(data_layout);

  auto unit = LLVMModuleCreateWithNameInContext("eko", llvm_context);
  LLVMSetTarget(unit, target_triple);
  LLVMSetDataLayout(unit, data_layout_str);

  for (auto &decl: file.top_level) {
    switch (decl->kind) {
      case Binding::Lambda: {
        auto &lambda = decl->lambda_binding;
        auto &name   = lambda.node->name;
        
        auto return_type = get_llvm_type(unit, lambda.return_type);

        auto params_count = lambda.params.count;
        auto params       = reinterpret_cast<LLVMTypeRef *>(alloca(params_count * sizeof(LLVMTypeRef)));

        auto function_type = LLVMFunctionType(return_type, params, params_count, false);
        auto function      = LLVMAddFunction(unit, name.value, function_type);

        auto it = lambda.entries.first;
        while (it) {
          auto block = LLVMAppendBasicBlock(function, nullptr);

          auto builder = LLVMCreateBuilder();
          LLVMPositionBuilderAtEnd(builder, block);

          while (it) {
            auto &entry = it->value;

            switch (entry.kind) {
              case Entry::Return: {
                auto &ret_entry = entry.return_entry;
                auto &ret_value = ret_entry.value;
                
                switch (ret_value.kind) {
                  case Value::Immediate: {
                    auto value_type = get_llvm_type(unit, ret_value.immediate.type);
                    LLVMBuildRet(builder, LLVMConstInt(value_type, ret_value.immediate.value, false));    
                    break;
                  }
                  case Value::Memory: {
                    fin_ensure(false && "UNIMPLEMENTED");
                    break;
                  }
                }
                
                

                break;
              }
            }

            it = it->next;
          }

          LLVMDisposeBuilder(builder);
        }


        break;
      }
      default: {
        fin_ensure(false && "UNSUPPORTED");
      }
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
