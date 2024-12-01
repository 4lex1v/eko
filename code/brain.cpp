
#include "anyfin/hash_table.hpp"
#include "anyfin/result.hpp"
#include "anyfin/option.hpp"

#include "eko.hpp"
#include "ast.hpp"
#include "brain.hpp"
#include "utils.hpp"

struct Basic_Types {
  constexpr static auto void_type = Type { .kind = Type::Built_In, .built_in_type = Built_In_Type::Void };
  constexpr static auto bool_type = Type { .kind = Type::Built_In, .built_in_type = Built_In_Type::Bool };
  
  constexpr static auto s8_type   = Type { .kind = Type::Built_In, .built_in_type = Built_In_Type::Signed_Byte };
  constexpr static auto s16_type  = Type { .kind = Type::Built_In, .built_in_type = Built_In_Type::Signed_Half_Word };
  constexpr static auto s32_type  = Type { .kind = Type::Built_In, .built_in_type = Built_In_Type::Signed_Word };
  constexpr static auto s64_type  = Type { .kind = Type::Built_In, .built_in_type = Built_In_Type::Signed_Double_Word };
 
  constexpr static auto u8_type   = Type { .kind = Type::Built_In, .built_in_type = Built_In_Type::Unsigned_Byte };
  constexpr static auto u16_type  = Type { .kind = Type::Built_In, .built_in_type = Built_In_Type::Unsigned_Half_Word };
  constexpr static auto u32_type  = Type { .kind = Type::Built_In, .built_in_type = Built_In_Type::Unsigned_Word };
  constexpr static auto u64_type  = Type { .kind = Type::Built_In, .built_in_type = Built_In_Type::Unsigned_Double_Word };
};

static int64_t signed_min (int bits) { return -(1LL << (bits - 1)); }
static int64_t signed_max (int bits) { return  (1LL << (bits - 1)) - 1; }

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

  Result<void> typecheck () {
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

    return Fin::Ok();
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

  Result<Type> typecheck_expression (const Scope &enclosing, const Expression_Node &expr) {
    switch (expr.expr_kind) {
      case Expression_Node::Literal: {
        auto &lit_node = expr.literal_expr;

        if (lit_node.value.kind == Token::String_Literal) {
          
        }

        if (lit_node.value.kind == Token::Integer_Literal) {
          if (lit_node.is_signed) {
            auto &value = lit_node.sint_value;
            
            if (value >= signed_min(8)  && value <= signed_max(8))  return Type(Basic_Types::s8_type);
            if (value >= signed_min(16) && value <= signed_max(16)) return Type(Basic_Types::s16_type);
            if (value >= signed_min(32) && value <= signed_max(32)) return Type(Basic_Types::s32_type);
            else                                                    return Type(Basic_Types::s64_type);
          }

          auto &value = lit_node.uint_value;

          if (value <= static_cast<u8>(-1))  return Type(Basic_Types::u8_type);
          if (value <= static_cast<u16>(-1)) return Type(Basic_Types::u16_type);
          if (value <= static_cast<u32>(-1)) return Type(Basic_Types::u32_type);
          else                               return Type(Basic_Types::u64_type);
        }
        
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

  bool types_are_the_same (const Type &left, const Type &right) {
    if (left.kind != right.kind) return false;

    if (left.kind == Type::Pointer || left.kind == Type::Seq)
      return types_are_the_same(*left.element, *right.element);

    if (left.kind == Type::Array) {
      // TODO: Figure out how to compare bounding expression results
      return types_are_the_same(*left.element, *right.element);
    }

    if (left.kind == Type::Struct) return left.binding == right.binding;

    /*
      These are basic the same basic types...?
      TODO: An interesting question is how to deal with upcasts?
     */
    return true;
  }

  bool int_value_type_can_fit (const Type &storage_type, const Type &value_type) {
    if (storage_type.kind != Type::Built_In || value_type.kind != Type::Built_In)
      return false;
    
    if (value_type.built_in_type == Built_In_Type::Signed_Byte) {
      return ((storage_type.built_in_type == Built_In_Type::Signed_Byte) ||
              (storage_type.built_in_type == Built_In_Type::Signed_Half_Word) ||
              (storage_type.built_in_type == Built_In_Type::Signed_Word) ||
              (storage_type.built_in_type == Built_In_Type::Signed_Double_Word));
    }

    if (value_type.built_in_type == Built_In_Type::Signed_Half_Word) {
      return ((storage_type.built_in_type == Built_In_Type::Signed_Half_Word) ||
              (storage_type.built_in_type == Built_In_Type::Signed_Word) ||
              (storage_type.built_in_type == Built_In_Type::Signed_Double_Word));
    }

    if (value_type.built_in_type == Built_In_Type::Signed_Word) {
      return ((storage_type.built_in_type == Built_In_Type::Signed_Word) ||
              (storage_type.built_in_type == Built_In_Type::Signed_Double_Word));
    }

    if (value_type.built_in_type == Built_In_Type::Signed_Double_Word) {
      return (storage_type.built_in_type == Built_In_Type::Signed_Double_Word);
    }

    if (value_type.built_in_type == Built_In_Type::Unsigned_Byte) {
      return ((storage_type.built_in_type == Built_In_Type::Unsigned_Byte) ||
              (storage_type.built_in_type == Built_In_Type::Unsigned_Half_Word) ||
              (storage_type.built_in_type == Built_In_Type::Unsigned_Word) ||
              (storage_type.built_in_type == Built_In_Type::Unsigned_Double_Word));
    }

    if (value_type.built_in_type == Built_In_Type::Unsigned_Half_Word) {
      return ((storage_type.built_in_type == Built_In_Type::Unsigned_Half_Word) ||
              (storage_type.built_in_type == Built_In_Type::Unsigned_Word) ||
              (storage_type.built_in_type == Built_In_Type::Unsigned_Double_Word));
    }

    if (value_type.built_in_type == Built_In_Type::Unsigned_Word) {
      return ((storage_type.built_in_type == Built_In_Type::Unsigned_Word) ||
              (storage_type.built_in_type == Built_In_Type::Unsigned_Double_Word));
    }

    if (value_type.built_in_type == Built_In_Type::Unsigned_Double_Word) {
      return (storage_type.built_in_type == Built_In_Type::Unsigned_Double_Word);
    }

    return false;
  }

  Result<Fin::List<Entry>> transform_expression (Expression_Node &node) {
    
  }

  Result<Entry> typecheck_statement (const Lambda_Binding &context, Statement_Node &node) {
    switch (node.stmnt_kind) {
      case Statement_Node::Return: {
        try(expr_result_type, typecheck_expression(context.scope, node.return_stmnt.value));

        if (!types_are_the_same(context.return_type, expr_result_type)) {
          if (!int_value_type_can_fit(context.return_type, expr_result_type))
            return Typer_Error();
        }

        try(entries, transform_expression(node.return_stmnt.value));

        return Entry(Return_Entry(entries));
      }
    }

    return Typer_Error();
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

Result<void> typecheck (Fin::Memory_Arena &arena, Source_File &file) {
  Typer typer(arena, file);
  return typer.typecheck();
}

