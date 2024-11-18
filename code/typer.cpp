
struct Typer {
  Type_Table types{};
  std::vector<Binding *> bindings{};

  const Type *typecheck_type (const Type_Node *type) {
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
        if (basic_type)
          return basic_type;

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
  }

  std::vector<Binding *>
  typecheck_parameters (const std::vector<Parameter_Node> &params) {
    std::vector<Binding *> result{};

    for (auto &param: params) {
      auto param_binding = new Variable_Binding(param.name);
      param_binding->type = typecheck_type(param.type);

      result.push_back(param_binding);
    }

    return result;
  }

  Binding *typecheck_lambda_binding (const Lambda_Decl_Node &lambda_decl) {
    auto lambda = new Lambda_Binding(lambda_decl.name);

    lambda->params = std::move(typecheck_parameters(lambda_decl.params));
    lambda->return_type = typecheck_type(lambda_decl.return_type);

    for (auto &node: lambda_decl.body) {
    }

    return lambda;
  }

  void process_node (const Node &node) {
    switch (node.kind) {
      case Node::Decl_Struct: {
        auto struct_decl = static_cast<const Struct_Decl_Node &>(node);

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

        break;
      }

      case Node::Decl_Lambda: {
        auto lambda_decl = static_cast<const Lambda_Decl_Node &>(node);

        this->bindings.push_back(typecheck_lambda_binding(lambda_decl));

        break;
      }

      default: {
        INCOMPLETE();
      }
    }
  }

  void process (Root_Node &root) {
    for (auto decl: root.decls)
      process_node(*decl);
  }
};
