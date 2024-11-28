
#include "anyfin/base.hpp"
#include "anyfin/result.hpp"

#include "tokenizer.hpp"
#include "utils.hpp"

static bool is_whitespace (char value) {
  return value == ' ' || value == '\t' || value == '\r' || value == '\n';
}

static bool is_numeric (u8 value) {
  return value >= '0' && value <= '9';
}

static bool is_alpha (u8 value) {
  return (value >= 'a' && value <= 'z') || (value >= 'A' && value <= 'Z');
}

static bool is_hex_digit (u8 c) {
  return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

using Result = Fin::Result<Tokenizer_Status, Token>;

struct Tokenizer {
  Source_File &unit;
  const char *current { unit.buffer.values };

  u16 row    = 0;
  u16 column = 0;

  /*
    Tokenizer Functions
  */

  template <typename T = Fin::String>
  [[nodiscard]] Token make_token (Token_Kind kind, T value = {}) const {
    Token token { .kind = kind, .row = row, .col = column };
    new (&token.value) T(Fin::move(value));
    return token;
  }

  [[nodiscard]] bool looking_at (u8 value) const { return *current == value; }

  /*
    Returns true if the last symbol in the file is reached, i.e \0
  */
  [[nodiscard]] bool advance (u32 by = 1) {
    for (u32 i = 0; i < by; i++) {
      if (*current == '\0') return true;
      if (*current == '\n') {
        row += 1;
        column = 0;
      }

      current++;
    }

    return false;
  }

  Result invalid_token () const {
    return Tokenizer_Status::Invalid_Token;
  }

  Result unclosed_string_literal () const {
    return Tokenizer_Status::Unclosed_String_Literal;
  }

  void skip_basic_whitespace () {
    while (*current == ' ' || *current == '\t')
      current += 1;
  }

  Result read_symbolic_token () {
    const char *value_start = current;
    while (true) {
      if (*current == '_' || is_alpha(*current) || is_numeric(*current)) {
        if (advance()) break;
        continue;
      }

      break;
    }

    auto value = Fin::String(value_start, current - value_start);
    if (value == "return") return make_token(Token::Return);
    if (value == "struct") return make_token(Token::Struct);
    if (value == "use")    return make_token(Token::Use);

    auto first_char = value[0];
    if (is_numeric(first_char))                     return invalid_token();
    if (!is_alpha(first_char) && first_char != '_') return invalid_token();

    return make_token(Token::Symbol, value);
  }

  Result read_numeric_token () {
    fin_ensure(is_numeric(*current));

    auto value_start = current;

    // Check for hexadecimal, binary, or octal literals
    if (*current == '0') {
      (void) advance();
      if (*current == 'x' || *current == 'X') {
        (void) advance();
        while (is_hex_digit(*current)) {
          (void) advance();
        }
      } else if (*current == 'b' || *current == 'B') {
        (void) advance();
        while (*current == '0' || *current == '1') {
          (void) advance();
        }
      } else if (is_numeric(*current)) {
        while (*current >= '0' && *current <= '7') {
          (void) advance();
        }
      } else {
        // Single digit '0' literal
      }
    } else {
      // Decimal literals
      while (is_numeric(*current)) {
        (void) advance();
      }
    }

    return make_token(Token::Integer_Literal, Fin::String(value_start, current - value_start));
  }

  Result read_string_literal () {
    fin_ensure(looking_at('"'));

    auto value_start = current;
    if (advance()) return unclosed_string_literal();

    /*
      TODO: This need a proper function that handles a string literal with all
      the escapes and other stuff
    */
    while (true) {
      auto value = *current;
      if (value == '"') {
        if (advance()) return unclosed_string_literal();

        break;
      }

      if (advance()) return unclosed_string_literal();
    }

    /*
      At this point the tokenizer is one symbol after the string literal closing
      quote, so no additional advances are needed.
    */

    auto token_value = Fin::String(value_start, current - value_start);
    return make_token(Token::String_Literal, token_value);
  }

  Result read_token () {
    skip_basic_whitespace();

    if (looking_at('\0')) return make_token(Token::Last);

    if (is_numeric(*current))                  return read_numeric_token();
    if (looking_at('"'))                       return read_string_literal();

    switch (*current) {
      case '\r': return make_token(Token::CR);
      case '\n': return make_token(Token::Newline);
      case ':':  return make_token(Token::Colon);
      case '(':  return make_token(Token::Open_Round_Bracket);
      case ')':  return make_token(Token::Close_Round_Bracket);
      case '[':  return make_token(Token::Open_Square_Bracket);
      case ']':  return make_token(Token::Close_Square_Bracket);
      case '{':  return make_token(Token::Open_Curly_Bracket);
      case '}':  return make_token(Token::Close_Curly_Bracket);
      case ',':  return make_token(Token::Coma);
      case ';':  return make_token(Token::Semicolon);
      case '=':  return make_token(Token::Equal);
      case '.':  return make_token(Token::Period);
      case '+':  return make_token(Token::Plus);
      case '-':  return make_token(Token::Minus);
      case '*':  return make_token(Token::Star);
      case '_':  return make_token(Token::Underscore);
    }

    return read_symbolic_token();
  }
};

Tokenizer_Status read_tokens (Fin::Memory_Arena &arena, Source_File &unit) {
  Tokenizer tokenizer(unit);

  auto tokens        = get_memory_at_current_offset<Token>(arena);
  usize tokens_count = 0;

  while (true) {
    try(token, tokenizer.read_token());
    *reserve<Token>(arena) = token; 
    tokens_count += 1;

    if (token.kind == Token::Last) break;

    (void) tokenizer.advance();
  }

  unit.tokens = Fin::Array(tokens, tokens_count);

  return Tokenizer_Status::Success;
}
