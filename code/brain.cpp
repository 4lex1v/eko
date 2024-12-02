
#include "anyfin/hash_table.hpp"
#include "anyfin/result.hpp"
#include "anyfin/option.hpp"
#include "anyfin/bit_mask.hpp"

#include "eko.hpp"
#include "ast.hpp"
#include "brain.hpp"
#include "utils.hpp"

struct Basic_Types {
  using enum Type_Flags;
  
  constexpr static auto void_type = Type { .kind = Type::Built_In, .built_in_type = Built_In_Type::Void };

  constexpr static auto bool_type = Type { .kind = Type::Built_In, .built_in_type = Built_In_Type::Integer, .flags = Unsigned | Bit };

  constexpr static auto s8_type   = Type { .kind = Type::Built_In, .built_in_type = Built_In_Type::Integer, .flags = Byte };
  constexpr static auto s16_type  = Type { .kind = Type::Built_In, .built_in_type = Built_In_Type::Integer, .flags = Half_Word };
  constexpr static auto s32_type  = Type { .kind = Type::Built_In, .built_in_type = Built_In_Type::Integer, .flags = Word };
  constexpr static auto s64_type  = Type { .kind = Type::Built_In, .built_in_type = Built_In_Type::Integer, .flags = Double_Word };
 
  constexpr static auto u8_type   = Type { .kind = Type::Built_In, .built_in_type = Built_In_Type::Integer, .flags = Unsigned | Byte };
  constexpr static auto u16_type  = Type { .kind = Type::Built_In, .built_in_type = Built_In_Type::Integer, .flags = Unsigned | Half_Word };
  constexpr static auto u32_type  = Type { .kind = Type::Built_In, .built_in_type = Built_In_Type::Integer, .flags = Unsigned | Word };
  constexpr static auto u64_type  = Type { .kind = Type::Built_In, .built_in_type = Built_In_Type::Integer, .flags = Unsigned | Double_Word };
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
      case Declaration_Node::Variable: { INCOMPLETE; return nullptr; }
      case Declaration_Node::Constant: { INCOMPLETE; return nullptr; }
    }
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

        switch (lit_node.lit_kind) {
          case Literal_Node::String: {
            break;
          }
          case Literal_Node::Signed_Integer: {
            auto &value = lit_node.sint_value;
            
            if (value >= signed_min(8)  && value <= signed_max(8))  return Type(Basic_Types::s8_type);
            if (value >= signed_min(16) && value <= signed_max(16)) return Type(Basic_Types::s16_type);
            if (value >= signed_min(32) && value <= signed_max(32)) return Type(Basic_Types::s32_type);
            else                                                    return Type(Basic_Types::s64_type);

            break;
          }
          case Literal_Node::Unsigned_Integer: {
            auto &value = lit_node.uint_value;

            if (value <= static_cast<u8>(-1))  return Type(Basic_Types::u8_type);
            if (value <= static_cast<u16>(-1)) return Type(Basic_Types::u16_type);
            if (value <= static_cast<u32>(-1)) return Type(Basic_Types::u32_type);
            else                               return Type(Basic_Types::u64_type);
            
            break;
          }
          case Literal_Node::Float: { INCOMPLETE; break; }
          case Literal_Node::Double: { INCOMPLETE; break; }
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
    if (left.kind != right.kind)                   return false;
    if (left.built_in_type != right.built_in_type) return false;

    if (left.kind == Type::Pointer || left.kind == Type::Seq)
      return types_are_the_same(*left.element, *right.element);

    if (left.kind == Type::Array) {
      // TODO: Figure out how to compare bounding expression results
      return types_are_the_same(*left.element, *right.element);
    }

    if (left.kind == Type::Struct) return left.binding == right.binding;

    fin_ensure(left == Type::Built_In);
    fin_ensure(right == Type::Built_In);

    return left.flags.bit_mask == right.flags.bit_mask;
  }

  bool int_value_type_can_fit (const Type &storage_type, const Type &value_type) {
    fin_ensure(storage_type == Built_In_Type::Integer);
    fin_ensure(value_type == Built_In_Type::Integer);

    using enum Type_Flags;

    if (storage_type.flags.is_set(Unsigned) != value_type.flags.is_set(Unsigned)) return false;

    return value_type.flags.bit_mask <= storage_type.flags.bit_mask;  
  }

  Result<Fin::List<Entry>> transform_expression (Expression_Node &node) {
    
  }
  
  Result<Entry> transform_statement (const Lambda_Binding &context, Statement_Node &node) {
    switch (node.stmnt_kind) {
      case Statement_Node::Return: {
        auto &return_expr = node.return_stmnt.value;
        
        try(expr_result_type, typecheck_expression(context.scope, return_expr));
        if (!types_are_the_same(context.return_type, expr_result_type)) {
          // TODO: Perhaps we would have to extend this at some later point
          if (expr_result_type != Built_In_Type::Integer)                     return Typer_Error();
          if (!int_value_type_can_fit(context.return_type, expr_result_type)) return Typer_Error();
            
          /*
            We need to upcast the number according to the declared return type.
          */
          expr_result_type = context.return_type;
        }

        if (return_expr == Expression_Node::Literal) {
          auto &lit_expr = return_expr.literal_expr;
          switch (lit_expr.lit_kind) {
            case Literal_Node::String: { break; }

            case Literal_Node::Unsigned_Integer:
            case Literal_Node::Signed_Integer: {
              return Entry(Return_Entry(Value(Immediate_Value {
                .imm_kind = Immediate_Value::Integer,
                .value    = lit_expr.uint_value,
                .type     = expr_result_type
              })));
            }
            
            case Literal_Node::Float: { break; }
            case Literal_Node::Double: { break; }
          }
        }

        try(entries, transform_expression(return_expr));
        fin_ensure(entries.last != nullptr);

        auto &last = entries.last->value;
        switch (last.kind) {
          case Entry::Load: return Entry(Return_Entry(Value(Memory_Value {
            .mem_kind   = Memory_Value::Load,
            .load_entry = static_cast<Load_Entry *>(&last.load_entry)
          })));
          default: return Typer_Error();
        }
      }
    }

    return Typer_Error();
  }

  Result<Binding *> typecheck_lambda (Scope &enclosing, Lambda_Node &lambda_node) {
    auto binding = new (arena) Binding(Lambda_Binding {
      .node  = &lambda_node,
      .scope = Scope(arena, &enclosing),
    });
    auto &lambda_binding = binding->lambda_binding;
    auto &scope          = lambda_binding.scope;
    
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
          try(entry, transform_statement(lambda_binding, node.stmnt_node));
          list_push(arena, lambda_binding.entries, Fin::move(entry));
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

