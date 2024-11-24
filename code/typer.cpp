
#include "anyfin/hash_table.hpp"
#include "anyfin/result.hpp"

#include "eko.hpp"
#include "ast.hpp"
#include "typer.hpp"
#include "utils.hpp"

struct Typecheck_Error {

};

template <typename T> using Result = Fin::Result<Typecheck_Error, T>;

struct Typer {
  Fin::Memory_Arena &arena;
  Source_File &file;

  void typecheck () {
    for (auto node: file.tree) {
      switch (node.kind) {
        case Node::Declaration: {
          switch (node.decl_node.decl_kind) {
            case Declaration_Node::Struct: {
              typecheck_struct(node.decl_node.struct_decl);
              break;
            }
            case Declaration_Node::Lambda: {
              typecheck_lambda(node.decl_node.lambda_decl);
              break;
            }
            default: {
              fin_ensure(false && "INCOMPLETE");
            }
          }
          break;
        }
        default: {
          fin_ensure(false && "INCOMPLETE");
        }
      }
    }
  }

  const Plain_Type_Node & get_underlying_type (const Type_Node &node) {
    switch (node.type_kind) {
      case Type_Node::Plain:   return node.plain_type;
      case Type_Node::Pointer: return get_underlying_type(node.pointer_type);
      case Type_Node::Array:   return get_underlying_type(node.array_type);
      case Type_Node::Seq:     return get_underlying_type(node.seq_type);
    }
  }

  usize hash_symbol (const Token &value) {
    return 0;
  }

  const Type * lookup_type_symbol (const Token &name) {

  }

  Result<Type> typecheck_type (const Type_Node &type_node) {
    auto raw  = get_underlying_type(type_node);
    auto type = lookup_type_symbol(raw.type_name);
    if (!type) {

    }
  }

  Result<Type> typecheck_expression (const Expression_Node &expr) {

  }

  Result<Struct_Binding> typecheck_struct (const Struct_Node &struct_decl) {
    auto binding = Struct_Binding { .node = &struct_decl, .scope = Scope(arena) };

    for (auto &field: struct_decl.fields) {
      // TODO: These are not supported at this point, but perhaps at some point this would make sense?
      if (field.decl_kind == Declaration_Node::Struct ||
          field.decl_kind == Declaration_Node::Lambda)
        return Typecheck_Error();

      if (field.decl_kind == Declaration_Node::Variable) {
        auto &var_decl = field.variable_decl;
        // As far the typer concerned we should have at least one of these. If
        // none of these is present, that should have been caught during parsing,
        // as invalid syntax.
        fin_ensure(var_decl.type || var_decl.expr);

        auto binding = Value_Binding { .node = &var_decl };

        if (var_decl.type) {
          try(type, typecheck_type(*var_decl.type));
          binding.type = type;
        }

        if (var_decl.expr) {
          try(expr_type, typecheck_expression(*var_decl.expr));
          binding.init_expr_type = expr_type;
        }
      }

    }
  }

  Result<Lambda_Binding> typecheck_lambda (const Lambda_Node &lambda_node) {

  }

};

void typecheck (Fin::Memory_Arena arena, Source_File &file) {
  Typer typer(arena, file);
  return typer.typecheck();
}

