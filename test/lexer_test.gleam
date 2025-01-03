import gleeunit/should
import lexer.{type Lexer}
import tokens

// ------------------------------------------------------------------------------------------------
// Lexer tests
// ------------------------------------------------------------------------------------------------

pub fn next_token_test() {
  "{};,=()"
  |> lexer.new_lexer
  |> expected_token("Token(Lbrace, {)")
  |> expected_token("Token(Rbrace, })")
  |> expected_token("Token(Semicolon, ;)")
  |> expected_token("Token(Comma, ,)")
  |> expected_token("Token(Assign, =)")
  |> expected_token("Token(Lparen, ()")
  |> expected_token("Token(Rparen, ))")
}

// ------------------------------------------------------------------------------------------------

pub fn ident_number_test() {
  "let fn 20 20.0"
  |> lexer.new_lexer
  |> expected_token("Token(Let, let)")
  |> expected_token("Token(Function, fn)")
  |> expected_token("Token(Int, 20)")
  |> expected_token("Token(Float, 20.0)")
}

// ------------------------------------------------------------------------------------------------

pub fn symbols_test() {
  "!= ! = == > >= < <= - + * /"
  |> lexer.new_lexer
  |> expected_token("Token(NotEq, !=)")
  |> expected_token("Token(Bang, !)")
  |> expected_token("Token(Assign, =)")
  |> expected_token("Token(Equal, ==)")
  |> expected_token("Token(GreaterThan, >)")
  |> expected_token("Token(GreaterEqual, >=)")
  |> expected_token("Token(LessThan, <)")
  |> expected_token("Token(LessEqual, <=)")
  |> expected_token("Token(Minus, -)")
  |> expected_token("Token(Plus, +)")
  |> expected_token("Token(Asterix, *)")
  |> expected_token("Token(Slash, /)")
}

// ------------------------------------------------------------------------------------------------

pub fn string_test() {
  "\"apples\""
  |> lexer.new_lexer
  |> expected_token("Token(String, apples)")
}

// ------------------------------------------------------------------------------------------------

fn expected_token(lexer: Lexer, expected: String) -> Lexer {
  let #(lexer, token) = lexer.next_token(lexer)
  token
  |> tokens.token_to_string
  |> should.equal(expected)

  lexer
}
// ------------------------------------------------------------------------------------------------
