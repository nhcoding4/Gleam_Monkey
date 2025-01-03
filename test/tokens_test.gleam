import gleeunit/should
import tokens.{type TokenType}

// ------------------------------------------------------------------------------------------------
// Tokens/TokenType tests 
// ------------------------------------------------------------------------------------------------

pub fn token_to_string_test() {
  test_token_string(tokens.ASSIGN, "=", "Token(Assign, =)")
  test_token_string(tokens.PLUS, "+", "Token(Plus, +)")
  test_token_string(tokens.MINUS, "-", "Token(Minus, -)")
  test_token_string(tokens.ASTERIX, "*", "Token(Asterix, *)")
  test_token_string(tokens.SLASH, "/", "Token(Slash, /)")
  test_token_string(tokens.LPAREN, "(", "Token(Lparen, ()")
  test_token_string(tokens.RPAREN, ")", "Token(Rparen, ))")
  test_token_string(tokens.LBRACE, "{", "Token(Lbrace, {)")
  test_token_string(tokens.RBRACE, "}", "Token(Rbrace, })")
  test_token_string(tokens.SEMICOLON, ";", "Token(Semicolon, ;)")
  test_token_string(tokens.COMMA, ",", "Token(Comma, ,)")
  test_token_string(tokens.BANG, "!", "Token(Bang, !)")
  test_token_string(tokens.EOF, "", "Token(Eof, )")
}

// ------------------------------------------------------------------------------------------------

fn test_token_string(t_type: TokenType, literal: String, expected: String) {
  tokens.Token(t_type, literal)
  |> tokens.token_to_string
  |> should.equal(expected)
}
// ------------------------------------------------------------------------------------------------
