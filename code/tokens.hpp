
#pragma once

#include "anyfin/base.hpp"
#include "anyfin/strings.hpp"

#include "utils.hpp"

enum struct Token_Kind: u8 {
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
  If,
  Else,
  As,
  Extern,
  Null,

  CR,
  Newline,

  Integer_Literal,
  Float_Literal,
  String_Literal,

  Symbol,
};

struct Token {
  using enum Token_Kind;

  Token_Kind kind = Undefined;

  u16 row, col;

  Fin::String value;

  GEN_KIND_CHECK(kind);
};

static bool operator == (const Token *token, Token_Kind kind) {
  return token->kind == kind;
}
