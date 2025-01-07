import ast
import evaluator
import gleam/erlang
import gleam/io
import gleam/list
import gleam/string
import lexer
import object
import parser

// ------------------------------------------------------------------------------------------------

pub fn repl(env: object.Environment) -> object.Environment {
  let input = erlang.get_line("> ")
  case input {
    Ok(val) -> {
      case string.length(val) {
        0 | 1 -> env
        _ -> {
          repl(parse_program(val, env))
        }
      }
    }
    Error(_) -> env
  }
}

// ------------------------------------------------------------------------------------------------

fn parse_program(input: String, env: object.Environment) -> object.Environment {
  let #(parser, program) =
    input
    |> lexer.new_lexer
    |> parser.new_parser
    |> parser.parse_program

  case parser.errors {
    [] -> {
      let #(object, env) = evaluator.eval(ast.ProgramNode(program), env)
      object
      |> object.inspect
      |> io.println

      env
    }
    _ -> {
      parser.errors
      |> list.each(fn(x: String) { io.println(x) })

      env
    }
  }
}
// ------------------------------------------------------------------------------------------------
