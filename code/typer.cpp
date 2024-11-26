
#include "anyfin/hash_table.hpp"
#include "anyfin/result.hpp"
#include "anyfin/option.hpp"

#include "eko.hpp"
#include "ast.hpp"
#include "typer.hpp"
#include "utils.hpp"

template <typename T> using Result = Fin::Result<Typer_Error, T>;

struct Basic_Types {
  constexpr static auto void_type = Type { .kind = Type::Basic_Void };
  constexpr static auto bool_type = Type { .kind = Type::Basic_Bool };
  constexpr static auto s32_type  = Type { .kind = Type::Basic_Signed_Word };
};

struct Typer {
  Fin::Memory_Arena &arena;
  Source_File       &file;

  Result<Binding *> typecheck_declaration (Scope &enclosing, Declaration_Node &node) {
    switch (node.decl_kind) {
      case Declaration_Node::Struct: return typecheck_struct(enclosing, node.struct_decl);
      case Declaration_Node::Lambda: return typecheck_lambda(enclosing, node.lambda_decl);
      default: {
        fin_ensure(false && "INCOMPLETE");
      }
    }

    return Typer_Error();
  }

  Fin::Option<Typer_Error> typecheck () {
    for (auto node: file.tree) {
      switch (node.kind) {
        case Node::Declaration: {
          try(binding, typecheck_declaration(file.scope, node.decl_node));
          list_push_copy(arena, file.top_level, binding);

          break;
        }
        default: {
          fin_ensure(false && "INCOMPLETE");
        }
      }
    }

    return Fin::None();
  }

  Fin::Option<Type> resolve_built_in_type (const Token &token) {
    fin_ensure(token.kind == Token::Symbol);

    if (token.value == "s32") return Type(Basic_Types::s32_type);

    return Fin::None();
  }
  
  Fin::Option<Type> find_type_declaration (const Scope &enclosing, const Plain_Type_Node &node) {
    auto lookup_result = enclosing.defs.find(node.type_name.value);
    if (lookup_result) {
      auto binding = *lookup_result;
      fin_ensure(binding->kind == Binding::Type);

      return Type {
        .kind    = Type::Struct,
        .binding = &binding->type_binding
      };
    }
    
    if (enclosing.parent == nullptr) return Fin::None();

    return find_type_declaration(*enclosing.parent, node);
  }

  Result<Type> typecheck_type (const Scope &enclosing, const Type_Node &node) {
    switch (node.type_kind) {
      case Type_Node::Plain: {
        auto plain = node.plain_type;

        auto built_in = resolve_built_in_type(plain.type_name);
        if (built_in) return built_in.take();

        auto lookup_result = find_type_declaration(enclosing, plain);
        if (lookup_result) return lookup_result.take();

        return Typer_Error();
      }
      case Type_Node::Pointer: {
        try(elem_type, typecheck_type(enclosing, *node.pointer_type.value_type));
        return Type { .kind = Type::Pointer, .element = new (arena) Type(elem_type) };
      }
      case Type_Node::Array: {
        try(boudns_type, typecheck_expression(enclosing, node.array_type.bounds));
        fin_ensure(false && "INCOMPLETE");

        break;
      }
      case Type_Node::Seq: {
        // TODO: Ensure that seq type is valid
        break;
      }
    }

    return Typer_Error();
  }

  Result<const Type *> typecheck_expression (const Scope &enclosing, const Expression_Node &expr) {
    switch (expr.expr_kind) {
      case Expression_Node::Literal: {
        auto &lit_node = expr.literal_expr;
        
        break;
      }
      default: {
        fin_ensure(false && "INCOMPLETE");
      }
    }
  }

  Result<Binding *> typecheck_struct (Scope &enclosing, Struct_Node &struct_decl) {
    auto binding = new (arena) Binding(Type_Binding {
      .node  = &struct_decl,
      .scope = Scope(arena, &enclosing),
    });
    auto &struct_binding = binding->type_binding;

    for (auto &field: struct_decl.fields) {
      // TODO: These are not supported at this point, but perhaps at some point this would make sense?
      if (field.decl_kind == Declaration_Node::Struct ||
          field.decl_kind == Declaration_Node::Lambda)
        return Typer_Error();

      if (field.decl_kind == Declaration_Node::Variable) {
        auto &var_decl = field.variable_decl;
        // As far the typer concerned we should have at least one of these. If
        // none of these is present, that should have been caught during parsing,
        // as invalid syntax.
        fin_ensure(var_decl.type || var_decl.expr);

        auto value_binding = Value_Binding { .node = &var_decl };

        if (var_decl.type) {
          try(type, typecheck_type(struct_binding.scope, *var_decl.type));
        }

        if (var_decl.expr) {
          typecheck_expression(struct_binding.scope, *var_decl.expr);
        }
      }

      enclosing.defs.insert_copy(struct_decl.name.value, binding);
    }

    return binding;
  }

  Result<Value_Binding> typecheck_variable (Scope &enclosing, Variable_Node &node) {
    if (node.type == nullptr) {
      if (node.expr == nullptr) {
        return Typer_Error();
      }

      typecheck_expression(enclosing, *node.expr);
    }
  }

  void typecheck_statement (const Lambda_Binding &context, Statement_Node &node) {
    switch (node.stmnt_kind) {
      case Statement_Node::Return: {
        typecheck_expression(context.scope, node.return_stmnt.value);
        break;
      }
    }
  }

  Result<Binding *> typecheck_lambda (Scope &enclosing, Lambda_Node &lambda_node) {
    auto binding = new (arena) Binding(Lambda_Binding {
      .node = &lambda_node,
      .scope = Scope(arena, &enclosing),
    });
    auto &lambda_binding = binding->lambda_binding;
    lambda_node.binding = &lambda_binding;

    auto &scope = lambda_binding.scope;
    
    for (auto &param: lambda_node.params) {
      try(var_binding, typecheck_variable(enclosing, param));
    }

    lambda_binding.return_type = Basic_Types::void_type;
    if (lambda_node.return_type) {
      try(result_type, typecheck_type(enclosing, *lambda_node.return_type));
      lambda_binding.return_type = result_type;
    }

    for (auto &node: lambda_node.body) {
      switch (node.kind) {
        case Node::Declaration: {
          try(binding, typecheck_declaration(lambda_binding.scope, node.decl_node));
          break;
        }
        case Node::Statement: {
          typecheck_statement(lambda_binding, node.stmnt_node);
          break;
        }
        case Node::Expression: {
          auto &expr = node.expr_node;

          typecheck_expression(lambda_binding.scope, expr);

          // TODO: Should I issue a warning that there's an unbound expression in the lambda's body?

          break;
        }
      }
    }

    

    return binding;
  }

};

Fin::Option<Typer_Error> typecheck (Fin::Memory_Arena &arena, Source_File &file) {
  Typer typer(arena, file);
  return typer.typecheck();
}

