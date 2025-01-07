import ast
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import lexer.{type Lexer}
import tokens.{type Token, type TokenType}

// ------------------------------------------------------------------------------------------------
// Precedence
// ------------------------------------------------------------------------------------------------

type Precedence {
  LOWEST
  EQUALS
  LESSGREATER
  SUM
  PRODUCT
  PREFIX
  CALL
}

// ------------------------------------------------------------------------------------------------

fn precedence_to_value(prec: Precedence) -> Int {
  case prec {
    LOWEST -> 1
    EQUALS -> 2
    LESSGREATER -> 3
    SUM -> 4
    PRODUCT -> 5
    PREFIX -> 6
    CALL -> 7
  }
}

// ------------------------------------------------------------------------------------------------
// Parser
// ------------------------------------------------------------------------------------------------

pub type Parser {
  Parser(
    lexer: Lexer,
    cur: Option(Token),
    peek: Option(Token),
    errors: List(String),
  )
}

// ------------------------------------------------------------------------------------------------

pub fn new_parser(lexer: Lexer) -> Parser {
  Parser(lexer, None, None, [])
  |> next_token
  |> next_token
}

// ------------------------------------------------------------------------------------------------
// Parsing functions
// ------------------------------------------------------------------------------------------------

pub fn parse_program(parser: Parser) -> #(Parser, ast.Program) {
  let #(parser, stmts) = build_stmts(parser, [])
  #(parser, ast.Program(stmts))
}

// ------------------------------------------------------------------------------------------------

fn build_stmts(
  parser: Parser,
  acc: List(ast.Statement),
) -> #(Parser, List(ast.Statement)) {
  case parser.cur {
    Some(tok) -> {
      case tok.token_type {
        tokens.EOF -> #(parser, acc)
        _ -> {
          let #(parser, stmt) = parse_statement(parser)
          case stmt {
            Some(val) ->
              build_stmts(next_token(parser), list.append(acc, [val]))
            None -> build_stmts(next_token(parser), acc)
          }
        }
      }
    }
    None -> #(parser, acc)
  }
}

// ------------------------------------------------------------------------------------------------

fn parse_statement(parser: Parser) -> #(Parser, Option(ast.Statement)) {
  case parser.cur {
    Some(tok) -> {
      case tok.token_type {
        tokens.LET -> parse_let_statement(parser)
        tokens.RETURN -> parse_return_statement(parser)
        _ -> parse_expression_statement(parser)
      }
    }
    None -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------
// Let statements
// ------------------------------------------------------------------------------------------------

fn parse_let_statement(parser: Parser) -> #(Parser, Option(ast.Statement)) {
  case parser.cur {
    Some(stmt) -> make_ident(parser, stmt)
    None -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------

fn make_ident(parser: Parser, stmt: Token) -> #(Parser, Option(ast.Statement)) {
  let #(parser, matching) = expect_peek(parser, tokens.IDENT)
  case matching {
    True -> {
      case parser.cur {
        Some(name) -> read_let_expr(parser, stmt, name)
        None -> #(parser, None)
      }
    }
    False -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------

fn read_let_expr(
  parser: Parser,
  stmt: Token,
  name: Token,
) -> #(Parser, Option(ast.Statement)) {
  let #(parser, matching) = expect_peek(parser, tokens.ASSIGN)
  case matching {
    True -> {
      let #(parser, stmt_value) = parse_expression(next_token(parser), LOWEST)
      case stmt_value {
        Some(val) -> {
          let parser = case peek_token_is(parser, tokens.SEMICOLON) {
            True -> next_token(parser)
            False -> parser
          }
          #(
            parser,
            Some(
              ast.StmtLet(ast.LetStatement(
                stmt,
                ast.Identifier(name, name.literal),
                val,
              )),
            ),
          )
        }
        None -> #(parser, None)
      }
    }
    False -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------
// Return Statement
// ------------------------------------------------------------------------------------------------

fn parse_return_statement(parser: Parser) -> #(Parser, Option(ast.Statement)) {
  case parser.cur {
    Some(return_tok) -> {
      let #(parser, return_val) = parse_expression(next_token(parser), LOWEST)
      case return_val {
        Some(val) -> {
          let parser = case peek_token_is(parser, tokens.SEMICOLON) {
            True -> next_token(parser)
            False -> parser
          }
          #(parser, Some(ast.StmtReturn(ast.ReturnStatement(return_tok, val))))
        }
        None -> #(parser, None)
      }
    }
    None -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------
// Expressions
// ------------------------------------------------------------------------------------------------
// Block expr 
// ------------------------------------------------------------------------------------------------

fn parse_block_statement(
  parser: Parser,
) -> #(Parser, Option(ast.BlockStatement)) {
  case parser.cur {
    Some(tok) -> {
      let #(parser, stmts) = build_block_stmts(next_token(parser), [])
      case list.length(stmts) {
        0 -> #(parser, None)
        _ -> #(parser, Some(ast.BlockStatement(tok, stmts)))
      }
    }
    None -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------

fn build_block_stmts(
  parser: Parser,
  acc: List(ast.Statement),
) -> #(Parser, List(ast.Statement)) {
  case !cur_tok_is(parser, tokens.RBRACE) && !cur_tok_is(parser, tokens.EOF) {
    True -> {
      let #(parser, stmt) = parse_statement(parser)
      case stmt {
        Some(state) ->
          build_block_stmts(next_token(parser), list.append(acc, [state]))
        None -> build_block_stmts(next_token(parser), acc)
      }
    }
    False -> #(parser, acc)
  }
}

// ------------------------------------------------------------------------------------------------
// Boolean
// ------------------------------------------------------------------------------------------------

fn parse_boolean(parser: Parser) -> #(Parser, Option(ast.Expression)) {
  case parser.cur {
    Some(tok) -> #(
      parser,
      Some(ast.ExprBool(ast.Boolean(tok, cur_tok_is(parser, tokens.TRUE)))),
    )
    None -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------
// Call expr 
// ------------------------------------------------------------------------------------------------

fn parse_call_expression(
  parser: Parser,
  func: ast.Expression,
) -> #(Parser, Option(ast.Expression)) {
  case parser.cur {
    Some(tok) -> {
      let #(parser, args) = parse_call_args(parser)
      #(parser, Some(ast.ExprCall(ast.CallExpression(tok, func, args))))
    }
    None -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------

fn parse_call_args(parser: Parser) -> #(Parser, List(ast.Expression)) {
  case peek_token_is(parser, tokens.RPAREN) {
    True -> #(next_token(parser), [])
    False -> {
      let #(parser, arg) = parse_expression(next_token(parser), LOWEST)
      case arg {
        Some(argument) -> make_arguments(parser, [argument])
        None -> #(parser, [])
      }
    }
  }
}

// ------------------------------------------------------------------------------------------------

fn make_arguments(
  parser: Parser,
  acc: List(ast.Expression),
) -> #(Parser, List(ast.Expression)) {
  case peek_token_is(parser, tokens.COMMA) {
    True -> {
      let parser =
        parser
        |> next_token
        |> next_token

      let #(parser, arg) = parse_expression(parser, LOWEST)

      case arg {
        Some(argument) -> make_arguments(parser, list.append(acc, [argument]))
        None -> make_arguments(parser, acc)
      }
    }
    False -> {
      let #(parser, matching) = expect_peek(parser, tokens.RPAREN)
      case matching {
        True -> #(parser, acc)
        False -> #(parser, [])
      }
    }
  }
}

// ------------------------------------------------------------------------------------------------
// Expr statement 
// ------------------------------------------------------------------------------------------------

fn parse_expression_statement(
  parser: Parser,
) -> #(Parser, Option(ast.Statement)) {
  case parser.cur {
    Some(tok) -> {
      let #(parser, expression) = parse_expression(parser, LOWEST)
      case expression {
        Some(expr) -> {
          let expr_stmt = Some(ast.StmtExpr(ast.ExpressionStatement(tok, expr)))
          case peek_token_is(parser, tokens.SEMICOLON) {
            True -> #(next_token(parser), expr_stmt)
            False -> #(parser, expr_stmt)
          }
        }
        None -> #(parser, None)
      }
    }
    None -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------

fn parse_expression(
  parser: Parser,
  prec: Precedence,
) -> #(Parser, Option(ast.Expression)) {
  case parser.cur {
    Some(tok) -> {
      case prefix_parsing_fn(tok) {
        Some(fun) -> {
          let #(parser, left_expr) = fun(parser)
          case left_expr {
            Some(left) -> {
              let #(parser, expr) = build_expr(parser, left, prec)
              #(parser, Some(expr))
            }
            None -> #(parser, None)
          }
        }
        None -> #(no_prefix_parsing_fn(parser), None)
      }
    }
    None -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------

fn build_expr(
  parser: Parser,
  expr: ast.Expression,
  prec: Precedence,
) -> #(Parser, ast.Expression) {
  case
    !peek_token_is(parser, tokens.SEMICOLON)
    && precedence_to_value(prec) < precedence_to_value(peek_precedence(parser))
  {
    True -> make_infix_expr(parser, expr, prec)
    False -> #(parser, expr)
  }
}

// ------------------------------------------------------------------------------------------------

fn make_infix_expr(
  parser: Parser,
  expr: ast.Expression,
  prec: Precedence,
) -> #(Parser, ast.Expression) {
  case parser.peek {
    Some(tok) -> {
      case infix_parsing_fn(tok) {
        Some(func) -> {
          let #(parser, parsed_expr) = func(next_token(parser), expr)
          case parsed_expr {
            Some(expr) -> build_expr(parser, expr, prec)
            None -> #(parser, expr)
          }
        }
        None -> #(parser, expr)
      }
    }
    None -> #(parser, expr)
  }
}

// ------------------------------------------------------------------------------------------------
// Float 
// ------------------------------------------------------------------------------------------------

fn parse_float_literal(parser: Parser) -> #(Parser, Option(ast.Expression)) {
  case parser.cur {
    Some(tok) -> {
      case float.parse(tok.literal) {
        Ok(val) -> #(parser, Some(ast.ExprFloat(ast.FloatingPoint(tok, val))))
        Error(_) -> #(number_error(parser, tokens.FLOAT), None)
      }
    }
    None -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------
// Function 
// ------------------------------------------------------------------------------------------------

fn parse_function_literal(parser: Parser) -> #(Parser, Option(ast.Expression)) {
  case parser.cur {
    Some(tok) -> get_function_parameters(parser, tok)
    None -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------

fn get_function_parameters(
  parser: Parser,
  literal: Token,
) -> #(Parser, Option(ast.Expression)) {
  let #(parser, matching) = expect_peek(parser, tokens.LPAREN)

  case matching {
    True -> {
      let #(parser, parameters) = parse_function_parameters(parser, [])
      case parameters {
        Some(para) -> get_function_body(parser, literal, para)
        None -> #(parser, None)
      }
    }
    False -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------

fn get_function_body(
  parser: Parser,
  literal: Token,
  parameters: List(ast.Identifier),
) -> #(Parser, Option(ast.Expression)) {
  let #(parser, matching) = expect_peek(parser, tokens.LBRACE)

  case matching {
    True -> {
      let #(parser, body) = parse_block_statement(parser)
      case body {
        Some(bod) -> {
          #(
            parser,
            Some(ast.ExprFunc(ast.FunctionLiteral(literal, parameters, bod))),
          )
        }
        None -> #(parser, None)
      }
    }
    False -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------

fn parse_function_parameters(
  parser: Parser,
  acc: List(ast.Identifier),
) -> #(Parser, Option(List(ast.Identifier))) {
  case peek_token_is(parser, tokens.RPAREN) {
    True -> #(next_token(parser), Some(acc))
    False -> {
      let parser = next_token(parser)
      case parser.cur {
        Some(tok) -> {
          let #(parser, idents) =
            build_other_func_parameters(parser, [
              ast.Identifier(tok, tok.literal),
            ])
          let #(parser, matching) = expect_peek(parser, tokens.RPAREN)
          case matching {
            True -> #(parser, Some(idents))
            False -> #(parser, None)
          }
        }
        None -> #(parser, Some(acc))
      }
    }
  }
}

// ------------------------------------------------------------------------------------------------

fn build_other_func_parameters(
  parser: Parser,
  acc: List(ast.Identifier),
) -> #(Parser, List(ast.Identifier)) {
  case peek_token_is(parser, tokens.COMMA) {
    True -> {
      let parser =
        parser
        |> next_token
        |> next_token

      case parser.cur {
        Some(tok) ->
          build_other_func_parameters(
            parser,
            list.append(acc, [ast.Identifier(tok, tok.literal)]),
          )
        None -> #(parser, acc)
      }
    }
    False -> #(parser, acc)
  }
}

// ------------------------------------------------------------------------------------------------
// Grouped expr 
// ------------------------------------------------------------------------------------------------

fn parse_grouped_expression(parser: Parser) -> #(Parser, Option(ast.Expression)) {
  let #(parser, expr) = parse_expression(next_token(parser), LOWEST)
  let #(parser, matching) = expect_peek(parser, tokens.RPAREN)
  case matching {
    True -> #(parser, expr)
    False -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------
// Ident 
// ------------------------------------------------------------------------------------------------

fn parse_identifier(parser: Parser) -> #(Parser, Option(ast.Expression)) {
  case parser.cur {
    Some(tok) -> #(
      parser,
      Some(ast.ExprIdent(ast.Identifier(tok, tok.literal))),
    )
    None -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------
// If expr 
// ------------------------------------------------------------------------------------------------

fn parse_if_expression(parser: Parser) -> #(Parser, Option(ast.Expression)) {
  case parser.cur {
    Some(tok) -> make_if_cond(parser, tok)
    None -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------

fn make_if_cond(
  parser: Parser,
  expr_tok: Token,
) -> #(Parser, Option(ast.Expression)) {
  let #(parser, matching) = expect_peek(parser, tokens.LPAREN)
  case matching {
    True -> {
      let #(parser, cond) = parse_expression(next_token(parser), LOWEST)
      case cond {
        Some(condition) -> {
          let #(parser, correct) = assert_correct_tokens_if(parser)
          case correct {
            True -> parse_consequence_if(parser, expr_tok, condition)
            False -> #(parser, None)
          }
        }
        None -> #(parser, None)
      }
    }
    False -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------

fn assert_correct_tokens_if(parser: Parser) -> #(Parser, Bool) {
  let #(parser, matching) = expect_peek(parser, tokens.RPAREN)
  case matching {
    True -> {
      let #(parser, matching) = expect_peek(parser, tokens.LBRACE)
      case matching {
        True -> #(parser, True)
        False -> #(parser, False)
      }
    }
    False -> #(parser, False)
  }
}

// ------------------------------------------------------------------------------------------------

fn parse_consequence_if(
  parser: Parser,
  expr_tok: Token,
  cond: ast.Expression,
) -> #(Parser, Option(ast.Expression)) {
  let #(parser, consq) = parse_block_statement(parser)
  case consq {
    Some(cons) -> {
      let #(parser, keep, blk) = alternative_if(parser)
      case keep {
        True -> #(
          parser,
          Some(ast.ExprIf(ast.IfExpression(expr_tok, cond, cons, blk))),
        )
        False -> #(parser, None)
      }
    }
    None -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------

fn alternative_if(parser: Parser) -> #(Parser, Bool, Option(ast.BlockStatement)) {
  case peek_token_is(parser, tokens.ELSE) {
    True -> {
      let #(parser, matching) = expect_peek(next_token(parser), tokens.LBRACE)
      case matching {
        True -> {
          let #(parser, alternative) = parse_block_statement(parser)
          case alternative {
            Some(block) -> {
              #(parser, True, Some(block))
            }
            None -> #(parser, True, None)
          }
        }
        False -> #(parser, False, None)
      }
    }
    False -> #(parser, True, None)
  }
}

// ------------------------------------------------------------------------------------------------
// Infix expr 
// ------------------------------------------------------------------------------------------------

fn parse_infix_expression(
  parser: Parser,
  left: ast.Expression,
) -> #(Parser, Option(ast.Expression)) {
  case parser.cur {
    Some(tok) -> {
      let precedence = cur_precedence_is(parser)
      let #(parser, right_expr) =
        parse_expression(next_token(parser), precedence)
      case right_expr {
        Some(right) -> #(
          parser,
          Some(
            ast.ExprInfix(ast.InfixExpression(tok, left, tok.literal, right)),
          ),
        )
        None -> #(parser, None)
      }
    }
    None -> #(next_token(parser), None)
  }
}

// ------------------------------------------------------------------------------------------------
// Int 
// ------------------------------------------------------------------------------------------------

fn parse_integer_literal(parser: Parser) -> #(Parser, Option(ast.Expression)) {
  case parser.cur {
    Some(tok) -> {
      case int.parse(tok.literal) {
        Ok(val) -> #(parser, Some(ast.ExprInt(ast.Integer(tok, val))))
        Error(_) -> #(number_error(parser, tokens.INT), None)
      }
    }
    None -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------
// Prefix expr 
// ------------------------------------------------------------------------------------------------

fn parse_prefix_expression(parser: Parser) -> #(Parser, Option(ast.Expression)) {
  case parser.cur {
    Some(tok) -> {
      let #(parser, right_expr) = parse_expression(next_token(parser), PREFIX)
      case right_expr {
        Some(right) -> #(
          parser,
          Some(ast.ExprPrefix(ast.PrefixExpression(tok, tok.literal, right))),
        )
        None -> #(parser, None)
      }
    }
    None -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------
// String 
// ------------------------------------------------------------------------------------------------

fn parse_string_literal(parser: Parser) -> #(Parser, Option(ast.Expression)) {
  case parser.cur {
    Some(tok) -> #(
      parser,
      Some(ast.ExprStr(ast.StringLiteral(tok, tok.literal))),
    )
    None -> #(parser, None)
  }
}

// ------------------------------------------------------------------------------------------------
// Associate Functions with tokens/precedences
// ------------------------------------------------------------------------------------------------

fn prefix_parsing_fn(
  token: Token,
) -> Option(fn(Parser) -> #(Parser, Option(ast.Expression))) {
  case token.token_type {
    tokens.BANG -> Some(parse_prefix_expression)
    tokens.FALSE -> Some(parse_boolean)
    tokens.FLOAT -> Some(parse_float_literal)
    tokens.FUNCTION -> Some(parse_function_literal)
    tokens.IDENT -> Some(parse_identifier)
    tokens.IF -> Some(parse_if_expression)
    tokens.INT -> Some(parse_integer_literal)
    tokens.LPAREN -> Some(parse_grouped_expression)
    tokens.MINUS -> Some(parse_prefix_expression)
    tokens.STRING -> Some(parse_string_literal)
    tokens.TRUE -> Some(parse_boolean)
    _ -> None
  }
}

// ------------------------------------------------------------------------------------------------

fn infix_parsing_fn(
  token: Token,
) -> Option(fn(Parser, ast.Expression) -> #(Parser, Option(ast.Expression))) {
  case token.token_type {
    tokens.PLUS
    | tokens.MINUS
    | tokens.SLASH
    | tokens.ASTERIX
    | tokens.EQ
    | tokens.NOTEQ
    | tokens.LESSEQUAL
    | tokens.GREATEREQUAL
    | tokens.LT
    | tokens.GT -> Some(parse_infix_expression)
    tokens.LPAREN -> Some(parse_call_expression)
    _ -> None
  }
}

// ------------------------------------------------------------------------------------------------

fn token_precedence(token: Token) -> Precedence {
  case token.token_type {
    tokens.EQ -> EQUALS
    tokens.NOTEQ -> EQUALS
    tokens.LPAREN -> CALL
    tokens.LT -> LESSGREATER
    tokens.GT -> LESSGREATER
    tokens.GREATEREQUAL -> LESSGREATER
    tokens.LESSEQUAL -> LESSGREATER
    tokens.PLUS -> SUM
    tokens.MINUS -> SUM
    tokens.SLASH -> PRODUCT
    tokens.ASTERIX -> PRODUCT
    _ -> LOWEST
  }
}

// ------------------------------------------------------------------------------------------------
// Advance tokens
// ------------------------------------------------------------------------------------------------

fn advance_until(parser: Parser, until: TokenType) -> Parser {
  case cur_tok_is(parser, until) {
    True -> parser
    False -> advance_until(next_token(parser), until)
  }
}

// ------------------------------------------------------------------------------------------------

fn expect_peek(parser: Parser, expected: TokenType) -> #(Parser, Bool) {
  case peek_token_is(parser, expected) {
    True -> #(next_token(parser), True)
    False -> #(peek_error(parser, expected), False)
  }
}

// ------------------------------------------------------------------------------------------------

fn next_token(parser: Parser) -> Parser {
  let #(lexer, next_token) = lexer.next_token(parser.lexer)
  Parser(..parser, lexer: lexer, cur: parser.peek, peek: Some(next_token))
}

// ------------------------------------------------------------------------------------------------
// Helpers 
// ------------------------------------------------------------------------------------------------

fn cur_precedence_is(parser: Parser) -> Precedence {
  case parser.cur {
    Some(tok) -> token_precedence(tok)
    None -> LOWEST
  }
}

// ------------------------------------------------------------------------------------------------

fn cur_tok_is(parser: Parser, expected: TokenType) -> Bool {
  case parser.cur {
    Some(tok) -> tok.token_type == expected
    None -> False
  }
}

// ------------------------------------------------------------------------------------------------

fn peek_precedence(parser: Parser) -> Precedence {
  case parser.peek {
    Some(tok) -> token_precedence(tok)
    None -> LOWEST
  }
}

// ------------------------------------------------------------------------------------------------

fn peek_token_is(parser: Parser, expected: TokenType) -> Bool {
  case parser.peek {
    Some(tok) -> tok.token_type == expected
    None -> False
  }
}

// ------------------------------------------------------------------------------------------------
// Errors 
// ------------------------------------------------------------------------------------------------

fn no_prefix_parsing_fn(parser: Parser) -> Parser {
  let tok_type_str = case parser.cur {
    Some(val) -> tokens.token_to_string(val)
    None -> "Nil"
  }

  let err_str =
    "Error: no prefix parsing func found for "
    <> tok_type_str
    <> " on line"
    <> int.to_string(parser.lexer.line)

  Parser(..parser, errors: list.append(parser.errors, [err_str]))
}

// ------------------------------------------------------------------------------------------------

fn number_error(parser: Parser, t_type: TokenType) -> Parser {
  let tok_literal = case parser.cur {
    Some(val) -> val.literal
    None -> "Nil"
  }

  let start_str = case t_type {
    tokens.INT -> "Error: could not parse integer literal on line "
    _ -> "Error: could not parse floating point number on line "
  }

  let err_str =
    start_str <> int.to_string(parser.lexer.line) <> ". Value: " <> tok_literal

  Parser(..parser, errors: list.append(parser.errors, [err_str]))
}

// ------------------------------------------------------------------------------------------------

fn peek_error(parser: Parser, expected: TokenType) -> Parser {
  let t_type = case parser.peek {
    Some(tok) -> tokens.tokentype_to_string(tok.token_type)
    None -> "Nil"
  }

  let err_str =
    "Error: wrong tokentype on line "
    <> int.to_string(parser.lexer.line)
    <> ". Wanted: "
    <> tokens.tokentype_to_string(expected)
    <> " but got: "
    <> t_type

  Parser(..parser, errors: list.append(parser.errors, [err_str]))
}
// ------------------------------------------------------------------------------------------------
