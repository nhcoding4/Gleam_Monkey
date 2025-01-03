pub type TokenType {
  ILLEGAL
  EOF

  IDENT
  INT
  FLOAT
  STRING

  ASSIGN
  PLUS
  MINUS
  ASTERIX
  SLASH

  COMMA
  SEMICOLON

  BANG
  LT
  LESSEQUAL
  GREATEREQUAL
  GT
  EQ
  NOTEQ

  LPAREN
  RPAREN
  LBRACE
  RBRACE

  FUNCTION
  LET
  TRUE
  FALSE
  IF
  ELSE
  RETURN
}

// ------------------------------------------------------------------------------------------------

pub fn tokentype_to_string(t_type: TokenType) -> String {
  case t_type {
    ILLEGAL -> "Illegal"
    EOF -> "Eof"

    IDENT -> "Ident"
    INT -> "Int"
    FLOAT -> "Float"
    STRING -> "String"

    ASSIGN -> "Assign"
    PLUS -> "Plus"
    MINUS -> "Minus"
    ASTERIX -> "Asterix"
    SLASH -> "Slash"

    COMMA -> "Comma"
    SEMICOLON -> "Semicolon"

    BANG -> "Bang"
    LT -> "LessThan"
    LESSEQUAL -> "LessEqual"
    GREATEREQUAL -> "GreaterEqual"
    GT -> "GreaterThan"
    EQ -> "Equal"
    NOTEQ -> "NotEq"

    LPAREN -> "Lparen"
    RPAREN -> "Rparen"
    LBRACE -> "Lbrace"
    RBRACE -> "Rbrace"

    FUNCTION -> "Function"
    LET -> "Let"
    TRUE -> "True"
    FALSE -> "False"
    IF -> "If"
    ELSE -> "Else"
    RETURN -> "Return"
  }
}

// ------------------------------------------------------------------------------------------------

pub fn lookup_ident(ident: String) -> TokenType {
  case ident {
    "fn" -> FUNCTION
    "let" -> LET
    "true" -> TRUE
    "false" -> FALSE
    "if" -> IF
    "else" -> ELSE
    "return" -> RETURN
    _ -> IDENT
  }
}

// ------------------------------------------------------------------------------------------------

pub type Token {
  Token(token_type: TokenType, literal: String)
}

// ------------------------------------------------------------------------------------------------

pub fn token_to_string(token: Token) -> String {
  "Token("
  <> tokentype_to_string(token.token_type)
  <> ", "
  <> token.literal
  <> ")"
}
// ------------------------------------------------------------------------------------------------
