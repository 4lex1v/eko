
#include "anyfin/hash_table.hpp"

#include "ast.hpp"

using namespace Fin;

namespace Eko {

struct Type {
  
};

struct Value_Binding {
  
};

struct Typer {
  const Type * typecheck_type (const Type_Node *type) {
#if 0
    switch (type->kind) {
      case Type_Node::Pointer: {
        auto pt_node = static_cast<const Pointer_Type_Node *>(type);

        auto pt = new Pointer_Type;
        pt->element_type = typecheck_type(pt_node->value_type);

        return pt;
      }

      case Type_Node::Plain: {
        auto pt = static_cast<const Plain_Type_Node *>(type);
        if (!pt->parameters.empty()) {
          INCOMPLETE();
          return nullptr;
        }

        auto basic_type = get_basic_type(pt->type_name);
        if (basic_type) return basic_type;

        /*
          If we are looking at the plain type, which is not basic, we need to
          make sure that this type is defined somewhere... It could be defined
          anywhere at this point, we could have seen it or not.

          For now, I'm not looking to add namespacing, so lets consider that all
          types are available in the global space. Thus we can say that this
          type is either defined or not and if we saw it or not...

          To simplify the development at this point, there's no out of order
          declaration supported, so we need to check that this type is defined
          by now.
        */
        if (auto result = this->types.find(pt->type_name.value);
            result != this->types.end()) {
          return result->second;
        }

        INCOMPLETE();

        return nullptr;
      }

      default: {
        INCOMPLETE();
        return nullptr;
      }
    }
#endif
  }

  const Type * typecheck_expression (const Node *expr) {
    
  }

  void ensure_types_equal (const Type *type_a, const Type *type_b) {
    if (type_a != type_b) fin_ensure(false && "Types are not equal");
  }

  void typecheck_parameter (const Parameter_Node &param) {
    if (!param.type && !param.init_expr) {
      // TODO: I need to do proper error handling at some point
      fin_ensure(false && "Invalid parameter declaration");
    }
    
    if (param.init_expr) {
      auto expr_type = typecheck_expression(param.init_expr);

      if (param.type) {
        auto param_type = typecheck_type(param.type);
        ensure_types_equal(param_type, expr_type);
      }
    }

    // auto binding = Value_Binding();
    
    // if (!param.type) {
    //   if (!param.init_expr) {
    //   }

    //   typecheck_expression(param.init_expr);
    // }

    // typecheck_type(param.type);
  }

  // std::vector<Binding *> typecheck_parameters (const List<Parameter_Node> &params) {
  //   std::vector<Binding *> result{};

  //   for (auto &param: params) {
  //     auto param_binding = new Variable_Binding(param.name);
  //     param_binding->type = typecheck_type(param.type);

  //     result.push_back(param_binding);
  //   }

  //   return result;
  // }

  // Binding * typecheck_lambda_binding (const Lambda_Decl_Node &lambda_decl) {
  //   auto lambda = new Lambda_Binding(lambda_decl.name);

  //   lambda->params = std::move(typecheck_parameters(lambda_decl.params));
  //   lambda->return_type = typecheck_type(lambda_decl.return_type);

  //   for (auto &node: lambda_decl.body) {
  //   }

  //   return lambda;
  // }

};

void typecheck (const Root_Node &root) {
  for (auto node: root.nodes) {
    if (node->kind == Node::Struct_Decl) {
#if 0
      auto struct_decl = &node.struct_decl;

      if (!struct_decl.params.empty()) {
        // TODO: I guess this is where this monomorphisation would happen?
        // Making a plain struct from a polimorphic one
        INCOMPLETE();
      }

      /*
        Step 1. Validate the struct declaration
        Step 2. Register it
      */
      auto struct_type = new Struct_Type;
      struct_type->fields = typecheck_parameters(struct_decl.fields);

      types[struct_decl.name.value] = struct_type;

      return;
#endif
    }
  }
}

}
