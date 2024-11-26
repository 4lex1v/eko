
#pragma once

#include "anyfin/base.hpp"
#include "anyfin/array.hpp"
#include "anyfin/list.hpp"
#include "anyfin/hash_table.hpp"
#include "anyfin/strings.hpp"

struct Node;
struct Token;
struct Binding;
struct Type;

struct Scope {
  const Scope *parent;

  Fin::Hash_Table<Fin::String, Binding *> defs;

  Scope (Fin::Allocator allocator, const Scope *_parent = nullptr):
    parent { _parent },
    defs   { allocator }
  {}
};

struct Module_Context {
  Scope scope;
};

struct Source_File {
  const char *file_path;
  const Fin::Array<char> buffer;

  const Module_Context *module;

  Fin::Array<Token> tokens;
  Fin::List<Node>   tree;

  Scope scope;
  
  Fin::List<Binding *> top_level;

  Source_File (Fin::Allocator allocator, const Module_Context *_module, const char *_file_path, Fin::Array<char> _buffer):
    file_path { _file_path },
    buffer    { _buffer },
    module    { _module },
    scope     { allocator, &module->scope }
  {}
};

