
#pragma once

#include "anyfin/base.hpp"
#include "anyfin/strings.hpp"

using Fin::String;

enum struct Token_Kind {
  Undefined,
  Last,

  Colon,
  Open_Round_Bracket,
  Close_Round_Bracket,
  Open_Square_Bracket,
  Close_Square_Bracket,
  Open_Curly_Bracket,
  Close_Curly_Bracket,
  Coma,
  Semicolon,
  Equal,
  Period,
  Plus,
  Minus,
  Star,
  Underscore,

  // Keywords
  Struct,
  Return,
  Use,
  And,
  Or,
  False,
  True,

  Newline,
  Numeric, // This should be refined into different types of literals
  // supported, integers and floats at least
  String_Literal,
  Symbol,
};

struct Token {
  using enum Token_Kind;
  
  Token_Kind kind = Undefined;

  String value;

  u32 row    = 0;
  u32 column = 0;
};

