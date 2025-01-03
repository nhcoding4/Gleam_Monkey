import ast
import gleam/erlang
import gleam/io
import gleam/list
import gleam/string
import lexer.{type Lexer}
import parser
import tokens

// ------------------------------------------------------------------------------------------------

pub fn repl() {
  let input = erlang.get_line("> ")
  case input {
    Ok(val) -> {
      case string.length(val) {
        0 | 1 -> Nil
        _ -> {
          parse_program(val)
          repl()
        }
      }
    }
    Error(_) -> Nil
  }
}

// ------------------------------------------------------------------------------------------------

fn read_tokens(lexer: Lexer) {
  case lexer.ch {
    "" -> Nil
    _ -> {
      let #(lexer, tok) = lexer.next_token(lexer)
      tok
      |> tokens.token_to_string
      |> io.println
      read_tokens(lexer)
    }
  }
}

// ------------------------------------------------------------------------------------------------

fn parse_program(input: String) {
  let #(parser, program) =
    input
    |> lexer.new_lexer
    |> parser.new_parser
    |> parser.parse_program

  program
  |> ast.program_to_string
  |> io.println()

  parser.errors
  |> list.each(fn(x: String) { io.println(x) })
}
// ------------------------------------------------------------------------------------------------
