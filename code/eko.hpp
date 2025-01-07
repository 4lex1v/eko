
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

/*
  TODO: This must be redesigned at some point, when we'll move towards multi-threaded architecture, things will blow up.
  Likely this would be a massive endeavour, as things are pretty much dependent on a global state of this arena...
 */
extern Fin::Memory_Arena global_arena;

struct Scope {
  const Scope *parent;

  Fin::String name;
  Fin::Hash_Table<Fin::String, Binding *> defs;

  /*
    Not sure if this constructor is correct here. I'm using scope in type_bindings, that are for both
    type declarations and struct declarations. For built-in types, that also use type_binding, there's
    no scope, since they are defined within the compiler, thus we need to create an empty scope.
    Perhaps this approach to type bindings would change at some point in the future and I can drop this
    constructor from here.
   */
  Scope () {};
  Scope (Fin::Allocator allocator, const Scope *_parent = nullptr, Fin::String _name = {}):
    parent { _parent },
    name   { Fin::move(_name) },
    defs   { allocator }
  {}

  const Binding * operator [] (const Fin::String &key) const {
    auto result = this->defs.find(key);
    return result ? *result : nullptr;
  }
};

struct Source_File {
  const char *file_path;
  const Fin::Array<char> buffer;

  Fin::Array<Token> tokens;
  Fin::List<Node>   tree;

  Scope scope;
  /*
    TODO: @speed
    Order of top level declarations stored in the scope's table  
   */
  Fin::List<Fin::String> top_level;

  Source_File (Fin::Allocator allocator, Scope &global, const char *_file_path, Fin::Array<char> _buffer):
    file_path { _file_path },
    buffer    { _buffer },
    scope     { allocator, &global, "file_scope" }
  {}
};

