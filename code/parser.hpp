
#pragma once

#include "anyfin/arena.hpp"
#include "anyfin/array.hpp"

#include "ast.hpp"

struct Parser_Error {
  enum Kind {
    Unexpected_Token
  };

  Kind kind;
};

Fin::Option<Parser_Error> build_tree (Fin::Memory_Arena &arena, Source_File &unit);
