
#pragma once

#include "anyfin/array.hpp"
#include "anyfin/result.hpp"

#include "eko.hpp"
#include "tokens.hpp"

enum struct Tokenizer_Status: u8 {
  Success,
  Unrecognized_Token,
  Invalid_Token,
  Unclosed_String_Literal
};

Tokenizer_Status read_tokens (Fin::Memory_Arena &arena, Source_File &unit);
