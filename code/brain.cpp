
#include "anyfin/hash_table.hpp"
#include "anyfin/prelude.hpp"
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

constexpr static struct { Fin::String name; const Type *type; } built_in_types [] {
  { "void", &Basic_Types::void_type },
  { "bool", &Basic_Types::bool_type },
  { "s8",   &Basic_Types::s8_type   },
  { "s16",  &Basic_Types::s16_type  },
  { "s32",  &Basic_Types::s32_type  },
  { "s64",  &Basic_Types::s64_type  },
  { "u8",   &Basic_Types::u8_type   },
  { "u16",  &Basic_Types::u16_type  },
  { "u32",  &Basic_Types::u32_type  },
  { "u64",  &Basic_Types::u64_type  },
};

static Scope      global_scope { global_arena };
static Type_Cache known_types  { global_arena };

void init_typer () {
  for (auto &[name, type]: built_in_types) {
    auto binding = new (global_arena) Binding(Type_Binding {
      .binding_kind = Type_Binding::Built_In,
      .type_value   = const_cast<Type *>(type),
    });

    known_types.insert_copy(name, binding);
  }
}

static int64_t signed_min (int bits) { return -(1LL << (bits - 1)); }
static int64_t signed_max (int bits) { return  (1LL << (bits - 1)) - 1; }

/*
  I just want call functions out-of-order without declaring them upfront...
 */
static struct Typer_Api {
  Result<void> typecheck (Source_File &file) {
    for (auto &node: file.tree) {
      switch (node.kind) {
        case Node::Declaration: {
          try(binding, typecheck_declaration(file.scope, node.decl_node));

          file.scope.defs.insert_copy(node.decl_node.name, binding);
          list_push_copy(global_arena, file.top_level, node.decl_node.name);

          break;
        }
        default: {
          INCOMPLETE;
        }
      }
    }

    return Fin::Ok();
  }
    
  Result<Binding *> typecheck_declaration (Scope &enclosing, Declaration_Node &node) {
    switch (node.decl_kind) {
      case Declaration_Node::Struct:   return typecheck_struct(enclosing, node.struct_decl);
      case Declaration_Node::Lambda:   return typecheck_lambda(enclosing, node.lambda_decl);
      case Declaration_Node::Constant: return typecheck_binding(enclosing, node.constant_decl);
      case Declaration_Node::Variable: { INCOMPLETE; return nullptr; }
    }
  }

  Fin::Option<Binding *> find_declaration (const Scope &enclosing, const Fin::String &name) {
    auto lookup_result = enclosing.defs.find(name);
    if (lookup_result) return Fin::move(*lookup_result);
    
    if (enclosing.parent == nullptr) return Fin::None();

    return find_declaration(*enclosing.parent, name);
  }

  Result<void> typecheck_ambiguous_as_type (const Scope &enclosing, Binding &binding) {
    fin_ensure(binding == Binding::Ambiguous);

    auto &expr = binding.ambiguous_binding.node->expr;
    switch (expr.expr_kind) {
      case Expression_Node::Identifier: {
        auto &id = expr.identifier_expr;
        
        auto *known_type = known_types.find(id.value);
        if (known_type) {
          fin_ensure((*known_type)->kind == Binding::Type);

          /*
            This would shortcut the alias chain to the actual type, which should be fine,
            given that it's a constant binding?
           */
          auto known_type_binding = &(*known_type)->type_binding;
          while (known_type_binding->binding_kind == Type_Binding::Alias) {
            known_type_binding = known_type_binding->alias;
          }

          fin_ensure(known_type_binding->binding_kind != Type_Binding::Alias);

          binding = Binding(Type_Binding {
            .binding_kind = Type_Binding::Alias,
            .alias        = known_type_binding,
          });

          return Fin::Ok();
        }

        auto lookup_result = find_declaration(enclosing, expr.identifier_expr.value);
        if (!lookup_result) return Typer_Error();

        auto &value_binding = *lookup_result.take();
        if (value_binding == Binding::Ambiguous) {
            typecheck_ambiguous_as_type(enclosing, value_binding);
        }
        
        return Typer_Error();
      }
      case Expression_Node::Star_Expr: {
        auto &star      = expr.star_expr;
        auto &type_expr = *star.expr;

        
        

        break;
      }
      default: {
        INCOMPLETE;
        break;
      }
    }

    return Fin::Ok();
  }

  Result<const Type *> typecheck_type (const Scope &enclosing, const Type_Node &node) {
    switch (node.type_kind) {
      case Type_Node::Plain: {
        auto plain = node.plain_type;

        auto *binding = known_types.find(plain.name);
        if (binding) return Fin::Ok<const Type *>((*binding)->type_binding.type_value);

        auto binding_lookup_result = find_declaration(enclosing, plain.name);
        if (binding_lookup_result) {
          auto &binding = *binding_lookup_result.take();
          if (binding == Binding::Type) {
            /*
              TODO: 
                This is some weird stuff happening here that I'm not sure about.
                If there's no such type with this name in the cache, it wasn't typechecked at this point.
             */
            INCOMPLETE;
            // auto &type_binding = binding.type_binding;
            // if (!type_binding.type_value) {
            //   type_binding.type_value = new (global_arena) Type {
            //     .kind    = Type::Struct,
            //     .binding = &type_binding
            //   };
            // }

            return Fin::Ok<const Type *>(binding.type_binding.type_value);
          }

          if (binding == Binding::Ambiguous) {
            fin_check(typecheck_ambiguous_as_type(enclosing, binding));
            fin_ensure(binding == Binding::Type);
            fin_ensure(binding.type_binding == Type_Binding::Alias);

            known_types.insert_copy(plain.name, &binding);

            return Fin::Ok<const Type *>(binding.type_binding.alias->type_value);
          }

          return Typer_Error(); // Found value is not a type declaration
        }

        return Typer_Error(); // No type declaration found
      }
      case Type_Node::Pointer: {
        try(elem_type, typecheck_type(enclosing, *node.pointer_type.value_type));
        return new (global_arena) Type { .kind = Type::Pointer, .element = elem_type };
      }
      case Type_Node::Array: {
        try(boudns_type, typecheck_expression(enclosing, node.array_type.bounds));
        INCOMPLETE;

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

        switch (lit_node.lit_kind) {
          case Literal_Node::Null:   { INCOMPLETE; break; }
          case Literal_Node::String: { INCOMPLETE; break; }
          case Literal_Node::Signed_Integer: {
            auto &value = lit_node.sint_value;
            
            if (value >= signed_min(8)  && value <= signed_max(8))  return &Basic_Types::s8_type;
            if (value >= signed_min(16) && value <= signed_max(16)) return &Basic_Types::s16_type;
            if (value >= signed_min(32) && value <= signed_max(32)) return &Basic_Types::s32_type;
            else                                                    return &Basic_Types::s64_type;

            break;
          }
          case Literal_Node::Unsigned_Integer: {
            auto &value = lit_node.uint_value;

            if (value <= static_cast<u8>(-1))  return &Basic_Types::u8_type;
            if (value <= static_cast<u16>(-1)) return &Basic_Types::u16_type;
            if (value <= static_cast<u32>(-1)) return &Basic_Types::u32_type;
            else                               return &Basic_Types::u64_type;
            
            break;
          }
          case Literal_Node::Float:  { INCOMPLETE; break; }
          case Literal_Node::Double: { INCOMPLETE; break; }
        }
        
        break;
      }
      case Expression_Node::As_Cast: {
        auto &as_cast = expr.as_cast_expr;

        try(expr_type, typecheck_expression(enclosing, *as_cast.expr));
        try(type,      typecheck_type(enclosing, *as_cast.type_node));

        if (expr_type->kind == type->kind) {
          /*
            For pointer casts I'm thinking of a c-like behaviour where we just reinterpret the memory at the
            pointer and fully trust the engineer that this operation is valid.
           */
          if (expr_type->kind == Type::Pointer) return type;

          if (expr_type->kind == Type::Built_In) {
            if (expr_type->built_in_type == Built_In_Type::Void) {
              // Attempt to cast void to void
              if (type->built_in_type == Built_In_Type::Void) return type;

              return Typer_Error(); // attempt to a void type into some other type
            }

            // Effectively discard the value?
            if (type->built_in_type == Built_In_Type::Void) return type;

            if (expr_type->built_in_type == Built_In_Type::String_Literal ||
                type->built_in_type == Built_In_Type::String_Literal) {
              /*
                TODO:
                  String literals are not implemented in the compiler at this point...
               */
              INCOMPLETE;
            }

            /*
              If none of the above conditions are handled, we must be looking at some case of numerics, ints of floats.
              I presume, we just return the latter and let the codegen handle the truncation or conversion?
             */
            return type;
          }
        }

        /*
          As for structs, arrays and seqs, not sure that language cast should work for these types, it's unclear how to do
          this kind of conversion here? Rather it should be something user-defined? How this could be integrated into the
          casting I don't know yet.
        */
        return Typer_Error();
      }
      default: {
        INCOMPLETE; // More commands to handle
        break;
      }
    }

    /*
      TODO:
        If the above code doesn't cover things there must be something fundamentaly wrong with this.
     */
    return Typer_Error();
  }

  Result<Binding *> typecheck_struct (Scope &enclosing, Struct_Node &struct_decl) {
    auto binding = new (global_arena) Binding(Type_Binding {
      .node  = &struct_decl,
      .scope = Scope(global_arena, &enclosing),
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

        auto value_binding = Value_Binding { };

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
    // First we need to deal with the type, it could either be provided by the user or inferred from the
    // init expression if the one is provided...
    if (node.type == nullptr) {
      if (node.expr == nullptr) return Typer_Error(); 

      try(expr_type, typecheck_expression(enclosing, *node.expr));

      return Value_Binding { .type = expr_type };
    }

    try(type_value, typecheck_type(enclosing, *node.type));
    auto binding = Value_Binding { .type = type_value };

    if (node.expr == nullptr) return binding;

    try(expr_type, typecheck_expression(enclosing, *node.expr));
    if (!types_are_the_same(*type_value, *expr_type)) return Typer_Error();

    return binding;
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
  
  Result<Entry> transform_statement (const Lambda_Binding &context, Statement_Node &node) {
    switch (node.stmnt_kind) {
      case Statement_Node::Return: {
        auto &return_expr = node.return_stmnt.value;
        
        try(expr_result_type, typecheck_expression(context.scope, return_expr));
        if (!types_are_the_same(*context.return_type, *expr_result_type)) {
          // TODO: Perhaps we would have to extend this at some later point
          if (expr_result_type->built_in_type != Built_In_Type::Integer)        return Typer_Error();
          if (!int_value_type_can_fit(*context.return_type, *expr_result_type)) return Typer_Error();
            
          /*
            We need to upcast the number according to the declared return type.
          */
          expr_result_type = context.return_type;
        }

        if (return_expr == Expression_Node::Literal) {
          auto &lit_expr = return_expr.literal_expr;
          switch (lit_expr.lit_kind) {
            case Literal_Node::Null: { INCOMPLETE; break; }
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

        INCOMPLETE; // I'm unclear about this code below, I should look more into this...

        //try(entries, transform_expression(return_expr));
        //fin_ensure(entries.last != nullptr);

        // auto &last = entries.last->value;
        // switch (last.kind) {
        //   case Entry::Load: return Entry(Return_Entry(Value(Memory_Value {
        //     .mem_kind   = Memory_Value::Load,
        //     .load_entry = static_cast<Load_Entry *>(&last.load_entry)
        //   })));
        //   default: return Typer_Error();
        // }
      }
    }

    return Typer_Error();
  }

  Result<Binding *> typecheck_lambda (Scope &enclosing, Lambda_Node &lambda_node) {
    auto binding = new (global_arena) Binding(Lambda_Binding {
      .node  = &lambda_node,
      .scope = Scope(global_arena, &enclosing),
    });
    auto &lambda_binding = binding->lambda_binding;
    auto &scope          = lambda_binding.scope;
    
    for (auto &param: lambda_node.params) {
      try(var_binding, typecheck_variable(enclosing, param));
    }

    lambda_binding.return_type = &Basic_Types::void_type;
    if (lambda_node.return_type) {
      try(expr_type, typecheck_type(enclosing, *lambda_node.return_type));
      lambda_binding.return_type = expr_type;
    }

    for (auto &node: lambda_node.body) {
      switch (node.kind) {
        case Node::Declaration: {
          try(binding, typecheck_declaration(lambda_binding.scope, node.decl_node));
          break;
        }
        case Node::Statement: {
          try(entry, transform_statement(lambda_binding, node.stmnt_node));
          list_push(global_arena, lambda_binding.entries, Fin::move(entry));
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

  Result<Binding *> typecheck_binding (Scope &enclosing, Constant_Node &node) {
    using enum Expression_Node_Kind;

    auto &expr = node.expr;

    /*
      TODO:
      I'm not sure how pointer dereferencing would work with constant bindings, will research this at some future point.
      For now I'm assuming that this could either be a pointer type binding or a pointer deref expression, which is ambiguous
      at the declaration site.
    */
    if (expr == Identifier || expr == Star_Expr) {
      return new (global_arena) Binding(Ambiguous_Binding(&node));
    }

    try(expr_type, typecheck_expression(enclosing, expr));
    return new (global_arena) Binding(Value_Binding {
      .type        = expr_type,
      .is_constant = true  
    });
  }
} typer;

Result<void> typecheck (Source_File &file) {
  return typer.typecheck(file);
}

