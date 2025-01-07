import gleam/list
import gleam/option.{type Option, None, Some}
import tokens.{type Token}

// ------------------------------------------------------------------------------------------------
// Interfaces
// ------------------------------------------------------------------------------------------------

pub type Node {
  ExprNode(Expression)
  StmtNode(Statement)
  ProgramNode(Program) 
}

// ------------------------------------------------------------------------------------------------

pub type Statement {
  StmtBlock(BlockStatement)
  StmtExpr(ExpressionStatement)
  StmtLet(LetStatement)
  StmtReturn(ReturnStatement)
  StmtPlaceholder
}

// ------------------------------------------------------------------------------------------------

pub type Expression {
  ExprBool(Boolean)
  ExprCall(CallExpression)
  ExprFloat(FloatingPoint)
  ExprFunc(FunctionLiteral)
  ExprIdent(Identifier)
  ExprIf(IfExpression)
  ExprInfix(InfixExpression)
  ExprInt(Integer)
  ExprPrefix(PrefixExpression)
  ExprStr(StringLiteral)
  ExprPlaceholder
}

// ------------------------------------------------------------------------------------------------
// Program
// ------------------------------------------------------------------------------------------------

pub type Program {
  Program(statements: List(Statement))
}

pub fn program_to_string(program: Program) -> String {
  statements_to_string("Program:", program.statements)
}

pub fn statements_to_string(
  start: String,
  statements: List(Statement),
) -> String {
  list.fold(statements, start <> "\n\t", fn(x: String, y: Statement) -> String {
    case y {
      StmtBlock(val) -> x <> block_stmt_to_string(val) <> "\n\t"
      StmtExpr(val) -> x <> expr_stmt_to_string(val) <> "\n\t"
      StmtLet(val) -> x <> letstmt_to_string(val) <> "\n\t"
      StmtReturn(val) -> x <> return_stmt_to_string(val) <> "\n\t"
      _ -> ""
    }
  })
}

// ------------------------------------------------------------------------------------------------
// Ast Nodes
// ------------------------------------------------------------------------------------------------
// Statements
// ------------------------------------------------------------------------------------------------

pub type BlockStatement {
  BlockStatement(token: Token, statements: List(Statement))
}

pub fn block_stmt_to_string(block: BlockStatement) -> String {
  tokens.token_to_string(block.token)
  <> "\n\t"
  <> statements_to_string("Block:", block.statements)
}

// ------------------------------------------------------------------------------------------------

pub type ExpressionStatement {
  ExpressionStatement(token: Token, expression: Expression)
}

pub fn expr_stmt_to_string(expr_stmt: ExpressionStatement) -> String {
  "expr_stmt("
  <> tokens.token_to_string(expr_stmt.token)
  <> ","
  <> expr_to_string(expr_stmt.expression)
  <> ")"
}

// ------------------------------------------------------------------------------------------------

pub type LetStatement {
  LetStatement(token: Token, name: Identifier, value: Expression)
}

pub fn letstmt_to_string(letstmt: LetStatement) -> String {
  "letstmt("
  <> tokens.token_to_string(letstmt.token)
  <> ", "
  <> identifier_to_string(letstmt.name)
  <> ", "
  <> expr_to_string(letstmt.value)
  <> ")"
}

// ------------------------------------------------------------------------------------------------

pub type ReturnStatement {
  ReturnStatement(token: Token, return_value: Expression)
}

pub fn return_stmt_to_string(return: ReturnStatement) -> String {
  "return_stmt("
  <> tokens.token_to_string(return.token)
  <> ", "
  <> expr_to_string(return.return_value)
  <> ")"
}

// ------------------------------------------------------------------------------------------------
// Expressions
// ------------------------------------------------------------------------------------------------

fn expr_to_string(expr: Expression) -> String {
  case expr {
    ExprBool(bool) -> "\n\t" <> bool_to_string(bool)
    ExprCall(call) -> "\n\t" <> call_expression_to_string(call)
    ExprFloat(float) -> "\n\t" <> float_to_string(float)
    ExprFunc(func) -> "\n\t" <> function_as_string(func)
    ExprIdent(ident) -> "\n\t" <> identifier_to_string(ident)
    ExprIf(if_expr) -> "\n\t" <> if_to_string(if_expr)
    ExprInfix(infix) -> "\n\t" <> infix_to_string(infix)
    ExprInt(int) -> "\n\t" <> integer_to_string(int)
    ExprPrefix(pre) -> "\n\t" <> prefix_expr_to_string(pre)
    ExprStr(str) -> "\n\t" <> string_literal_to_string(str)
    _ -> "Nil"
  }
}

// ------------------------------------------------------------------------------------------------

pub type Boolean {
  Boolean(token: Token, value: Bool)
}

pub fn bool_to_string(bool: Boolean) -> String {
  tokens.token_to_string(bool.token)
}

// ------------------------------------------------------------------------------------------------

pub type CallExpression {
  CallExpression(
    token: Token,
    function: Expression,
    arguments: List(Expression),
  )
}

pub fn call_expression_to_string(call_expr: CallExpression) -> String {
  tokens.token_to_string(call_expr.token)
  <> expr_to_string(call_expr.function)
  <> list.fold(
    call_expr.arguments,
    "\n\targs:\n\t",
    fn(x: String, y: Expression) { x <> expr_to_string(y) <> "\n\t" },
  )
}

// ------------------------------------------------------------------------------------------------

pub type FloatingPoint {
  FloatingPoint(token: Token, value: Float)
}

pub fn float_to_string(float: FloatingPoint) -> String {
  tokens.token_to_string(float.token)
}

// ------------------------------------------------------------------------------------------------

pub type FunctionLiteral {
  FunctionLiteral(
    token: Token,
    paramters: List(Identifier),
    body: BlockStatement,
  )
}

pub fn function_as_string(func: FunctionLiteral) -> String {
  tokens.token_to_string(func.token)
  <> list.fold(
    func.paramters,
    "\n\tparameters:\n\t",
    fn(x: String, y: Identifier) { x <> identifier_to_string(y) <> "\n\t" },
  )
  <> block_stmt_to_string(func.body)
}

// ------------------------------------------------------------------------------------------------

pub type Identifier {
  Identifier(token: Token, value: String)
}

pub fn identifier_to_string(ident: Identifier) -> String {
  tokens.token_to_string(ident.token)
}

// ------------------------------------------------------------------------------------------------

pub type IfExpression {
  IfExpression(
    token: Token,
    condition: Expression,
    consequence: BlockStatement,
    alternative: Option(BlockStatement),
  )
}

pub fn if_to_string(if_expr: IfExpression) -> String {
  tokens.token_to_string(if_expr.token)
  <> expr_to_string(if_expr.condition)
  <> block_stmt_to_string(if_expr.consequence)
  <> case if_expr.alternative {
    Some(alt) -> block_stmt_to_string(alt)
    None -> ""
  }
}

// ------------------------------------------------------------------------------------------------

pub type InfixExpression {
  InfixExpression(
    token: Token,
    left: Expression,
    operator: String,
    right: Expression,
  )
}

pub fn infix_to_string(infix: InfixExpression) -> String {
  tokens.token_to_string(infix.token)
  <> expr_to_string(infix.left)
  <> expr_to_string(infix.right)
}

// ------------------------------------------------------------------------------------------------

pub type Integer {
  Integer(token: Token, value: Int)
}

pub fn integer_to_string(integer: Integer) -> String {
  tokens.token_to_string(integer.token)
}

// ------------------------------------------------------------------------------------------------

pub type PrefixExpression {
  PrefixExpression(token: Token, operator: String, right: Expression)
}

pub fn prefix_expr_to_string(prefix: PrefixExpression) -> String {
  tokens.token_to_string(prefix.token) <> " " <> expr_to_string(prefix.right)
}

// ------------------------------------------------------------------------------------------------

pub type StringLiteral {
  StringLiteral(token: Token, value: String)
}

pub fn string_literal_to_string(str_lit: StringLiteral) -> String {
  tokens.token_to_string(str_lit.token)
}
// ------------------------------------------------------------------------------------------------
