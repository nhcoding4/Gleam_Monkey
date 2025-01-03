import gleam/int
import gleam/list
import gleam/string
import tokens.{type Token, type TokenType}

// ------------------------------------------------------------------------------------------------
// Lexer 
// ------------------------------------------------------------------------------------------------

pub type Lexer {
  Lexer(
    input: String,
    pos: Int,
    read: Int,
    line: Int,
    ch: String,
    errors: List(String),
  )
}

// ------------------------------------------------------------------------------------------------

pub fn new_lexer(input: String) {
  Lexer(input, 0, 0, 1, "", [])
  |> read_char
}

// ------------------------------------------------------------------------------------------------
// Token creation
// ------------------------------------------------------------------------------------------------

pub fn next_token(lexer: Lexer) -> #(Lexer, Token) {
  let lexer = skip_whitespace(lexer)
  case lexer.ch {
    "!" -> lex_bang(lexer)
    "=" -> lex_equal(lexer)
    ">" -> lex_greater_equal(lexer)
    "<" -> lex_less_equal(lexer)
    "\"" -> lex_string(lexer)
    ";" -> lex_generic(lexer, tokens.SEMICOLON)
    "(" -> lex_generic(lexer, tokens.LPAREN)
    ")" -> lex_generic(lexer, tokens.RPAREN)
    "{" -> lex_generic(lexer, tokens.LBRACE)
    "}" -> lex_generic(lexer, tokens.RBRACE)
    "," -> lex_generic(lexer, tokens.COMMA)
    "+" -> lex_generic(lexer, tokens.PLUS)
    "/" -> lex_generic(lexer, tokens.SLASH)
    "*" -> lex_generic(lexer, tokens.ASTERIX)
    "-" -> lex_generic(lexer, tokens.MINUS)
    "" -> lex_generic(lexer, tokens.EOF)
    _ -> lex_other(lexer)
  }
}

// ------------------------------------------------------------------------------------------------

fn lex_bang(lexer: Lexer) -> #(Lexer, Token) {
  case peek_char(lexer) == "=" {
    True -> {
      let lexer =
        lexer
        |> read_char
        |> read_char
      #(lexer, tokens.Token(tokens.NOTEQ, "!="))
    }
    False -> lex_generic(lexer, tokens.BANG)
  }
}

// ------------------------------------------------------------------------------------------------

fn lex_equal(lexer: Lexer) -> #(Lexer, Token) {
  case peek_char(lexer) == "=" {
    True -> {
      let lexer =
        lexer
        |> read_char
        |> read_char
      #(lexer, tokens.Token(tokens.EQ, "=="))
    }
    False -> lex_generic(lexer, tokens.ASSIGN)
  }
}

// ------------------------------------------------------------------------------------------------

fn lex_greater_equal(lexer: Lexer) -> #(Lexer, Token) {
  case peek_char(lexer) == "=" {
    True -> {
      let lexer =
        lexer
        |> read_char
        |> read_char
      #(lexer, tokens.Token(tokens.GREATEREQUAL, ">="))
    }
    False -> lex_generic(lexer, tokens.GT)
  }
}

// ------------------------------------------------------------------------------------------------

fn lex_generic(lexer: Lexer, t_type: TokenType) -> #(Lexer, Token) {
  #(read_char(lexer), tokens.Token(t_type, lexer.ch))
}

// ------------------------------------------------------------------------------------------------

fn lex_letter(lexer: Lexer) -> #(Lexer, Token) {
  let #(lexer, literal) = read_identifier(lexer, lexer.pos, 0)
  #(lexer, tokens.Token(tokens.lookup_ident(literal), literal))
}

// ------------------------------------------------------------------------------------------------

fn lex_less_equal(lexer: Lexer) -> #(Lexer, Token) {
  case peek_char(lexer) == "=" {
    True -> {
      let lexer =
        lexer
        |> read_char
        |> read_char
      #(lexer, tokens.Token(tokens.LESSEQUAL, "<="))
    }
    False -> lex_generic(lexer, tokens.LT)
  }
}

// ------------------------------------------------------------------------------------------------

fn lex_number(lexer: Lexer) -> #(Lexer, Token) {
  let #(lexer, literal) = read_number(lexer, lexer.pos, 0)
  case string.contains(literal, ".") {
    True -> #(lexer, tokens.Token(tokens.FLOAT, literal))
    False -> #(lexer, tokens.Token(tokens.INT, literal))
  }
}

// ------------------------------------------------------------------------------------------------

fn lex_other(lexer: Lexer) -> #(Lexer, Token) {
  case is_letter(lexer.ch) {
    True -> lex_letter(lexer)
    False -> {
      case is_digit(lexer.ch) {
        True -> lex_number(lexer)
        False -> lex_generic(lexer, tokens.ILLEGAL)
      }
    }
  }
}

// ------------------------------------------------------------------------------------------------

fn lex_string(lexer: Lexer) -> #(Lexer, Token) {
  let #(lexer, literal) =
    lexer
    |> read_char
    |> read_string(lexer.read, 0)
  #(lexer, tokens.Token(tokens.STRING, literal))
}

// ------------------------------------------------------------------------------------------------
// Advance characters
// ------------------------------------------------------------------------------------------------

fn read_char(lexer: Lexer) -> Lexer {
  case lexer.read >= string.length(lexer.input) {
    True -> Lexer(..lexer, pos: lexer.read, read: lexer.read + 1, ch: "")
    False -> {
      let new_ch = string.slice(lexer.input, lexer.read, 1)
      case new_ch == "\n" {
        True ->
          Lexer(
            ..lexer,
            pos: lexer.read,
            read: lexer.read + 1,
            line: lexer.line + 1,
            ch: new_ch,
          )
        False ->
          Lexer(..lexer, pos: lexer.read, read: lexer.read + 1, ch: new_ch)
      }
    }
  }
}

// ------------------------------------------------------------------------------------------------

fn read_identifier(lexer: Lexer, start: Int, length: Int) -> #(Lexer, String) {
  case is_letter(lexer.ch) {
    True -> {
      lexer
      |> read_char
      |> read_identifier(start, length + 1)
    }
    False -> #(lexer, string.slice(lexer.input, start, length))
  }
}

// ------------------------------------------------------------------------------------------------

fn read_number(lexer: Lexer, start: Int, length: Int) -> #(Lexer, String) {
  case is_digit(lexer.ch) {
    True -> {
      lexer
      |> read_char
      |> read_number(start, length + 1)
    }
    False -> #(lexer, string.slice(lexer.input, start, length))
  }
}

// ------------------------------------------------------------------------------------------------

fn read_string(lexer: Lexer, start: Int, length: Int) -> #(Lexer, String) {
  case lexer.ch == "\"" {
    True -> {
      let string = string.slice(lexer.input, start, length)
      #(read_char(lexer), string)
    }
    False -> {
      lexer
      |> read_char
      |> read_string(start, length + 1)
    }
  }
}

// ------------------------------------------------------------------------------------------------

fn skip_whitespace(lexer: Lexer) -> Lexer {
  case
    lexer.ch == " " || lexer.ch == "\n" || lexer.ch == "\t" || lexer.ch == "\r"
  {
    True -> {
      lexer
      |> read_char
      |> skip_whitespace
    }
    False -> lexer
  }
}

// ------------------------------------------------------------------------------------------------
// Helpers
// ------------------------------------------------------------------------------------------------

fn is_letter(ch: String) -> Bool {
  let unicode =
    ch
    |> string.to_utf_codepoints
    |> list.first

  case unicode {
    Ok(val) -> {
      let value = string.utf_codepoint_to_int(val)
      65 <= value && value <= 90 || 97 <= value && value <= 122 || value == 95
    }
    Error(_) -> False
  }
}

// ------------------------------------------------------------------------------------------------

fn is_digit(ch: String) -> Bool {
  case ch == "." {
    True -> True
    False -> {
      case int.parse(ch) {
        Ok(_) -> True
        Error(_) -> False
      }
    }
  }
}

// ------------------------------------------------------------------------------------------------

fn peek_char(lexer: Lexer) -> String {
  case lexer.read >= string.length(lexer.input) {
    True -> ""
    False -> string.slice(lexer.input, lexer.read, 1)
  }
}
// ------------------------------------------------------------------------------------------------
