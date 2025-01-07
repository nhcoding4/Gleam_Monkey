import ast.{type Expression, type Node, type Statement}
import gleam/option.{None, Some}
import object.{type Environment, type Object}

// ------------------------------------------------------------------------------------------------
// Entrypoints 
// ------------------------------------------------------------------------------------------------

pub fn eval(node: Node, env: Environment) -> #(Object, Environment) {
  case node {
    ast.ProgramNode(program) ->
      eval_program(program.statements, object.ObjNull(object.Null("")), env)
    ast.ExprNode(expr) -> eval_expr(expr, env)
    ast.StmtNode(stmt) -> eval_stmt(stmt, env)
  }
}

// ------------------------------------------------------------------------------------------------

fn eval_program(
  statements: List(Statement),
  acc: Object,
  env: Environment,
) -> #(Object, Environment) {
  case statements {
    [] -> #(acc, env)
    [head, ..tail] -> {
      let #(result, env) = eval_stmt(head, env)
      case result {
        object.ObjReturn(_) -> #(result, env)
        object.ObjErr(_) -> #(result, env)
        _ -> eval_program(tail, result, env)
      }
    }
  }
}

// ------------------------------------------------------------------------------------------------

fn eval_stmt(stmt: Statement, env: Environment) -> #(Object, Environment) {
  case stmt {
    ast.StmtExpr(expr_stmt) -> eval_expr(expr_stmt.expression, env)
    ast.StmtBlock(block_stmt) ->
      eval_block_statement(
        block_stmt.statements,
        object.ObjNull(object.Null("null")),
        env,
      )
    ast.StmtReturn(return_stmt) -> {
      let #(val, env) = eval_expr(return_stmt.return_value, env)
      case is_error(val) {
        True -> #(val, env)
        False -> #(object.ObjReturn(object.Return(val)), env)
      }
    }
    ast.StmtLet(let_stmt) -> {
      let #(val, env) = eval_expr(let_stmt.value, env)
      case is_error(val) {
        True -> #(val, env)
        False -> {
          let #(env, val) = object.set(env, let_stmt.name.value, val)
          #(val, env)
        }
      }
    }
    _ -> #(new_error("Error: could not parse statement"), env)
  }
}

// ------------------------------------------------------------------------------------------------

fn eval_block_statement(
  statements: List(Statement),
  acc: Object,
  env: Environment,
) -> #(Object, Environment) {
  case statements {
    [] -> #(acc, env)
    [head, ..tail] -> {
      let #(result, env) = eval_stmt(head, env)
      case object.object_type(result) {
        object.ReturnObj -> #(result, env)
        object.ErrObj -> #(result, env)
        _ -> eval_block_statement(tail, result, env)
      }
    }
  }
}

// ------------------------------------------------------------------------------------------------

fn eval_expr(expr: Expression, env: Environment) -> #(Object, Environment) {
  case expr {
    ast.ExprBool(bool) -> #(object.ObjBool(object.Boolean(bool.value)), env)
    ast.ExprFloat(float) -> #(
      object.ObjFloat(object.FloatingPoint(float.value)),
      env,
    )
    ast.ExprIf(if_expr) -> eval_if_expr(if_expr, env)
    ast.ExprInt(integer) -> #(object.ObjInt(object.Integer(integer.value)), env)
    ast.ExprStr(str) -> #(object.ObjStr(object.Str(str.value)), env)
    ast.ExprPrefix(prefix) -> eval_pre(prefix, env)
    ast.ExprInfix(infix) -> eval_infix(infix, env)
    ast.ExprIdent(ident) -> eval_ident(ident, env)
    _ -> #(new_error("Error: could not parse expression"), env)
  }
}

// ------------------------------------------------------------------------------------------------
// Identifiers
// ------------------------------------------------------------------------------------------------

fn eval_ident(expr: ast.Identifier, env: Environment) -> #(Object, Environment) {
  let #(val, ok) = object.get(env, expr.value)
  case ok {
    True -> #(val, env)
    False -> #(new_error("error: Identifier not found " <> expr.value), env)
  }
}

// ------------------------------------------------------------------------------------------------
// Prefix expr eval
// ------------------------------------------------------------------------------------------------

fn eval_pre(
  prefix: ast.PrefixExpression,
  env: Environment,
) -> #(Object, Environment) {
  let #(right, env) = eval_expr(prefix.right, env)
  case is_error(right) {
    True -> #(right, env)
    False -> #(eval_prefix(prefix.operator, right), env)
  }
}

// ------------------------------------------------------------------------------------------------

fn eval_prefix(operator: String, right: Object) -> Object {
  case operator {
    "!" -> eval_bang_operator(right)
    "-" -> eval_minus_prefix_operator(right)
    _ ->
      new_error(
        "Error: invalid prefix operator -> applied "
        <> operator
        <> " to type: "
        <> right |> object.object_type |> object.object_type_to_string,
      )
  }
}

// ------------------------------------------------------------------------------------------------

fn eval_bang_operator(right: Object) -> Object {
  case right {
    object.ObjBool(bool) -> {
      case bool.value {
        True -> object.ObjBool(object.Boolean(False))
        False -> object.ObjBool(object.Boolean(True))
      }
    }
    object.ObjNull(_) -> object.ObjBool(object.Boolean(True))
    _ -> object.ObjBool(object.Boolean(False))
  }
}

// ------------------------------------------------------------------------------------------------

fn eval_minus_prefix_operator(right: Object) -> Object {
  case right {
    object.ObjInt(integer) -> object.ObjInt(object.Integer(-integer.value))
    object.ObjFloat(float) ->
      object.ObjFloat(object.FloatingPoint(float.value *. -1.0))

    _ ->
      new_error(
        "Error: minus prefix applied to invalid type -> "
        <> right |> object.object_type |> object.object_type_to_string,
      )
  }
}

// ------------------------------------------------------------------------------------------------
// Infix expr evaluation
// ------------------------------------------------------------------------------------------------

fn eval_infix(
  infix: ast.InfixExpression,
  env: Environment,
) -> #(Object, Environment) {
  let #(left, env) = eval_expr(infix.left, env)
  case is_error(left) {
    True -> #(left, env)
    False -> {
      let #(right, env) = eval_expr(infix.right, env)
      case is_error(right) {
        True -> #(right, env)
        False -> #(eval_infix_expression(infix.operator, left, right), env)
      }
    }
  }
}

// ------------------------------------------------------------------------------------------------

fn eval_infix_expression(
  operator: String,
  left: Object,
  right: Object,
) -> Object {
  case left {
    object.ObjInt(int_left) -> {
      case right {
        object.ObjInt(int_right) ->
          eval_integer_infix(operator, int_left.value, int_right.value)
        _ -> infix_error(operator, left, right)
      }
    }
    object.ObjFloat(float_left) -> {
      case right {
        object.ObjFloat(float_right) ->
          eval_float_infix(operator, float_left.value, float_right.value)
        _ -> infix_error(operator, left, right)
      }
    }
    object.ObjStr(str_left) -> {
      case right {
        object.ObjStr(str_right) ->
          eval_string_infix(operator, str_left.value, str_right.value)
        _ -> infix_error(operator, left, right)
      }
    }
    object.ObjBool(bool_left) -> {
      case right {
        object.ObjBool(bool_right) ->
          eval_bool_infix(operator, bool_left.value, bool_right.value)
        _ -> infix_error(operator, left, right)
      }
    }
    _ -> infix_error(operator, left, right)
  }
}

// ------------------------------------------------------------------------------------------------

fn eval_integer_infix(operator: String, left: Int, right: Int) -> Object {
  case operator {
    "+" -> object.ObjInt(object.Integer(left + right))
    "-" -> object.ObjInt(object.Integer(left - right))
    "*" -> object.ObjInt(object.Integer(left * right))
    "/" -> object.ObjInt(object.Integer(left / right))
    ">" -> object.ObjBool(object.Boolean(left > right))
    ">=" -> object.ObjBool(object.Boolean(left >= right))
    "<" -> object.ObjBool(object.Boolean(left < right))
    "<=" -> object.ObjBool(object.Boolean(left <= right))
    "==" -> object.ObjBool(object.Boolean(left == right))
    _ -> new_error("error: invalid infix operator detected: " <> operator)
  }
}

// ------------------------------------------------------------------------------------------------

fn eval_float_infix(operator: String, left: Float, right: Float) -> Object {
  case operator {
    "+" -> object.ObjFloat(object.FloatingPoint(left +. right))
    "-" -> object.ObjFloat(object.FloatingPoint(left -. right))
    "*" -> object.ObjFloat(object.FloatingPoint(left *. right))
    "/" -> object.ObjFloat(object.FloatingPoint(left /. right))
    ">" -> object.ObjBool(object.Boolean(left >. right))
    ">=" -> object.ObjBool(object.Boolean(left >=. right))
    "<" -> object.ObjBool(object.Boolean(left <. right))
    "<=" -> object.ObjBool(object.Boolean(left <=. right))
    "==" -> object.ObjBool(object.Boolean(left == right))
    _ -> new_error("error: invalid infix operator detected: " <> operator)
  }
}

// ------------------------------------------------------------------------------------------------

fn eval_string_infix(operator: String, left: String, right: String) -> Object {
  case operator {
    "+" -> object.ObjStr(object.Str(left <> right))
    "==" -> object.ObjBool(object.Boolean(left == right))
    "!=" -> object.ObjBool(object.Boolean(left != right))
    _ -> new_error("error: invalid infix operator detected: " <> operator)
  }
}

// ------------------------------------------------------------------------------------------------

fn eval_bool_infix(operator: String, left: Bool, right: Bool) -> Object {
  case operator {
    "==" -> object.ObjBool(object.Boolean(left == right))
    "!=" -> object.ObjBool(object.Boolean(left != right))
    _ -> new_error("error: invalid infix operator detected: " <> operator)
  }
}

// ------------------------------------------------------------------------------------------------
// If expr
// ------------------------------------------------------------------------------------------------

fn eval_if_expr(
  if_expr: ast.IfExpression,
  env: Environment,
) -> #(Object, Environment) {
  let #(cond, env) = eval(ast.ExprNode(if_expr.condition), env)
  case is_error(cond) {
    True -> #(cond, env)
    False ->
      case is_truthy(cond) {
        True -> eval(ast.StmtNode(ast.StmtBlock(if_expr.consequence)), env)
        False -> {
          case if_expr.alternative {
            Some(alt) -> eval(ast.StmtNode(ast.StmtBlock(alt)), env)
            None -> #(
              new_error("error: no viable alternative found in if expression"),
              env,
            )
          }
        }
      }
  }
}

// ------------------------------------------------------------------------------------------------

fn is_truthy(obj: Object) -> Bool {
  case obj {
    object.ObjNull(_) -> False
    object.ObjBool(bool_obj) -> bool_obj.value
    _ -> True
  }
}

// ------------------------------------------------------------------------------------------------
// Errors
// ------------------------------------------------------------------------------------------------

fn new_error(message: String) -> Object {
  object.ObjErr(object.Error(message))
}

// ------------------------------------------------------------------------------------------------

fn is_error(obj: Object) -> Bool {
  case object.object_type(obj) {
    object.ErrObj -> True
    _ -> False
  }
}

// ------------------------------------------------------------------------------------------------

fn infix_error(operator: String, left: Object, right: Object) -> Object {
  new_error(
    "Error: invalid operation -> attempted: "
    <> left |> object.object_type |> object.object_type_to_string
    <> " "
    <> operator
    <> " "
    <> right |> object.object_type |> object.object_type_to_string,
  )
}
// ------------------------------------------------------------------------------------------------
