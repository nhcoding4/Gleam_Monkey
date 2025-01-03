import ast
import gleam/io
import gleam/list
import gleeunit/should
import lexer
import parser

// ------------------------------------------------------------------------------------------------
// Parser tests
// ------------------------------------------------------------------------------------------------

pub fn let_stmt_parsing_test() {
  parse_test(
    "let x = 5;",
    "Program:\n\tletstmt(Token(Let, let), Token(Ident, x), \n\tToken(Int, 5))\n\t",
  )
  parse_test(
    "let xyz = 80;",
    "Program:\n\tletstmt(Token(Let, let), Token(Ident, xyz), \n\tToken(Int, 80))\n\t",
  )
  parse_test(
    "let x_y_z = 15;",
    "Program:\n\tletstmt(Token(Let, let), Token(Ident, x_y_z), \n\tToken(Int, 15))\n\t",
  )
}

// ------------------------------------------------------------------------------------------------

pub fn return_test() {
  parse_test(
    "return 5;",
    "Program:\n\treturn_stmt(Token(Return, return), \n\tToken(Int, 5))\n\t",
  )
  parse_test(
    "return \"Apples\";",
    "Program:\n\treturn_stmt(Token(Return, return), \n\tToken(String, Apples))\n\t",
  )
}

// ------------------------------------------------------------------------------------------------

pub fn ident_test() {
  parse_test(
    "foobar",
    "Program:\n\texpr_stmt(Token(Ident, foobar),\n\tToken(Ident, foobar))\n\t",
  )
}

// ------------------------------------------------------------------------------------------------

pub fn parser_string_test() {
  parse_test(
    "\"foobar\"",
    "Program:\n\texpr_stmt(Token(String, foobar),\n\tToken(String, foobar))\n\t",
  )
}

// ------------------------------------------------------------------------------------------------

pub fn numbers_test() {
  parse_test(
    "20.0",
    "Program:\n\texpr_stmt(Token(Float, 20.0),\n\tToken(Float, 20.0))\n\t",
  )
  parse_test(
    "20",
    "Program:\n\texpr_stmt(Token(Int, 20),\n\tToken(Int, 20))\n\t",
  )
}

// ------------------------------------------------------------------------------------------------

pub fn prefix_test() {
  parse_test(
    "!20",
    "Program:\n\texpr_stmt(Token(Bang, !),\n\tToken(Bang, !) \n\tToken(Int, 20))\n\t",
  )

  parse_test(
    "-20.222",
    "Program:\n\texpr_stmt(Token(Minus, -),\n\tToken(Minus, -) \n\tToken(Float, 20.222))\n\t",
  )

  parse_test(
    "-\"Apples\"",
    "Program:\n\texpr_stmt(Token(Minus, -),\n\tToken(Minus, -) \n\tToken(String, Apples))\n\t",
  )
}

// ------------------------------------------------------------------------------------------------

pub fn infix_test() {
  parse_test(
    "10 + 20;",
    "Program:\n\texpr_stmt(Token(Int, 10),\n\tToken(Plus, +)\n\tToken(Int, 10)\n\tToken(Int, 20))\n\t",
  )
  parse_test(
    "10 - 20;",
    "Program:\n\texpr_stmt(Token(Int, 10),\n\tToken(Minus, -)\n\tToken(Int, 10)\n\tToken(Int, 20))\n\t",
  )
  parse_test(
    "10 * 20;",
    "Program:\n\texpr_stmt(Token(Int, 10),\n\tToken(Asterix, *)\n\tToken(Int, 10)\n\tToken(Int, 20))\n\t",
  )
  parse_test(
    "10 / 20;",
    "Program:\n\texpr_stmt(Token(Int, 10),\n\tToken(Slash, /)\n\tToken(Int, 10)\n\tToken(Int, 20))\n\t",
  )
  parse_test(
    "10 == 20;",
    "Program:\n\texpr_stmt(Token(Int, 10),\n\tToken(Equal, ==)\n\tToken(Int, 10)\n\tToken(Int, 20))\n\t",
  )
  parse_test(
    "10 < 20;",
    "Program:\n\texpr_stmt(Token(Int, 10),\n\tToken(LessThan, <)\n\tToken(Int, 10)\n\tToken(Int, 20))\n\t",
  )
  parse_test(
    "10 > 20;",
    "Program:\n\texpr_stmt(Token(Int, 10),\n\tToken(GreaterThan, >)\n\tToken(Int, 10)\n\tToken(Int, 20))\n\t",
  )
  parse_test(
    "10 >= 20;",
    "Program:\n\texpr_stmt(Token(Int, 10),\n\tToken(GreaterEqual, >=)\n\tToken(Int, 10)\n\tToken(Int, 20))\n\t",
  )
  parse_test(
    "10 <= 20;",
    "Program:\n\texpr_stmt(Token(Int, 10),\n\tToken(LessEqual, <=)\n\tToken(Int, 10)\n\tToken(Int, 20))\n\t",
  )
}

// ------------------------------------------------------------------------------------------------

pub fn bool_test() {
  parse_test(
    "3 < 5 == true",
    "Program:\n\texpr_stmt(Token(Int, 3),\n\tToken(Equal, ==)\n\tToken(LessThan, <)\n\tToken(Int, 3)\n\tToken(Int, 5)\n\tToken(True, true))\n\t",
  )

  parse_test(
    "3 > 5 != true",
    "Program:\n\texpr_stmt(Token(Int, 3),\n\tToken(NotEq, !=)\n\tToken(GreaterThan, >)\n\tToken(Int, 3)\n\tToken(Int, 5)\n\tToken(True, true))\n\t",
  )

  parse_test(
    "3 <= 5 == false",
    "Program:\n\texpr_stmt(Token(Int, 3),\n\tToken(Equal, ==)\n\tToken(LessEqual, <=)\n\tToken(Int, 3)\n\tToken(Int, 5)\n\tToken(False, false))\n\t",
  )
}

// ------------------------------------------------------------------------------------------------

pub fn group_expr_test() {
  parse_test(
    "2 / (3 + 3)",
    "Program:\n\texpr_stmt(Token(Int, 2),\n\tToken(Slash, /)\n\tToken(Int, 2)\n\tToken(Plus, +)\n\tToken(Int, 3)\n\tToken(Int, 3))\n\t",
  )
}

pub fn if_test() {
  parse_test(
    "if(x == 12){let x = 12;}",
    "Program:\n\texpr_stmt(Token(If, if),\n\tToken(If, if)\n\tToken(Equal, ==)\n\tToken(Ident, x)\n\tToken(Int, 12)Token(Lbrace, {)\n\tBlock:\n\tletstmt(Token(Let, let), Token(Ident, x), \n\tToken(Int, 12))\n\t)\n\t",
  )

  parse_test(
    "if(x == 12){let x = 12;} else {let x = 13; }",
    "Program:\n\texpr_stmt(Token(If, if),\n\tToken(If, if)\n\tToken(Equal, ==)\n\tToken(Ident, x)\n\tToken(Int, 12)Token(Lbrace, {)\n\tBlock:\n\tletstmt(Token(Let, let), Token(Ident, x), \n\tToken(Int, 12))\n\tToken(Lbrace, {)\n\tBlock:\n\tletstmt(Token(Let, let), Token(Ident, x), \n\tToken(Int, 13))\n\t)\n\t",
  )
}

// ------------------------------------------------------------------------------------------------

pub fn function_test() {
  parse_test(
    "fn(ay, ax){let x = 10;}",
    "Program:\n\texpr_stmt(Token(Function, fn),\n\tToken(Function, fn)\n\tparameters:\n\tToken(Ident, ay)\n\tToken(Ident, ax)\n\tToken(Lbrace, {)\n\tBlock:\n\tletstmt(Token(Let, let), Token(Ident, x), \n\tToken(Int, 10))\n\t)\n\t",
  )
}

// ------------------------------------------------------------------------------------------------

pub fn call_test() {
  parse_test(
    "add(a + b)",
    "Program:\n\texpr_stmt(Token(Ident, add),\n\tToken(Lparen, ()\n\tToken(Ident, add)\n\targs:\n\t\n\tToken(Plus, +)\n\tToken(Ident, a)\n\tToken(Ident, b)\n\t)\n\t",
  )
}

// ------------------------------------------------------------------------------------------------

pub fn parse_test(input: String, expected: String) {
  let #(parser, program) =
    input
    |> lexer.new_lexer
    |> parser.new_parser
    |> parser.parse_program

  parser
  |> print_parser_errors

  parser.errors
  |> list.length
  |> should.equal(0)

  program
  |> ast.program_to_string
  |> should.equal(expected)
}

// ------------------------------------------------------------------------------------------------

fn print_parser_errors(parser: parser.Parser) {
  case list.length(parser.errors) {
    0 -> Nil
    _ -> list.each(parser.errors, fn(x) { io.println_error(x) })
  }
}
// ------------------------------------------------------------------------------------------------
