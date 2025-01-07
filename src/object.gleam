import gleam/dict.{type Dict}
import gleam/float
import gleam/int

// ------------------------------------------------------------------------------------------------
// Object types 
// ------------------------------------------------------------------------------------------------

pub type ObjectType {
  BoolObj
  ErrObj
  FloatObj
  IntegerObj
  NullObj
  ReturnObj
  StrObj
}

// ------------------------------------------------------------------------------------------------

pub type Object {
  ObjBool(Boolean)
  ObjErr(Error)
  ObjFloat(FloatingPoint)
  ObjInt(Integer)
  ObjNull(Null)
  ObjReturn(Return)
  ObjStr(Str)
}

// ------------------------------------------------------------------------------------------------

pub type Boolean {
  Boolean(value: Bool)
}

// ------------------------------------------------------------------------------------------------

pub type Error {
  Error(message: String)
}

// ------------------------------------------------------------------------------------------------

pub type FloatingPoint {
  FloatingPoint(value: Float)
}

// ------------------------------------------------------------------------------------------------

pub type Integer {
  Integer(value: Int)
}

// ------------------------------------------------------------------------------------------------

pub type Null {
  Null(value: String)
}

// ------------------------------------------------------------------------------------------------

pub type Return {
  Return(value: Object)
}

// ------------------------------------------------------------------------------------------------

pub type Str {
  Str(value: String)
}

// ------------------------------------------------------------------------------------------------
// String representation
// ------------------------------------------------------------------------------------------------

pub fn inspect(object: Object) -> String {
  case object {
    ObjBool(bool) -> {
      case bool.value {
        True -> "true"
        False -> "false"
      }
    }
    ObjErr(error) -> error.message
    ObjInt(int) -> int.to_string(int.value)
    ObjFloat(float) -> float.to_string(float.value)
    ObjReturn(obj) -> inspect(obj.value)
    ObjStr(str) -> str.value
    ObjNull(_) -> "null"
  }
}

// ------------------------------------------------------------------------------------------------
// Type conversion
// ------------------------------------------------------------------------------------------------

pub fn object_type(object: Object) -> ObjectType {
  case object {
    ObjBool(_) -> BoolObj
    ObjErr(_) -> ErrObj
    ObjFloat(_) -> FloatObj
    ObjInt(_) -> IntegerObj
    ObjNull(_) -> NullObj
    ObjReturn(_) -> ReturnObj
    ObjStr(_) -> StrObj
  }
}

// ------------------------------------------------------------------------------------------------

pub fn object_type_to_string(o_type: ObjectType) -> String {
  case o_type {
    BoolObj -> "Boolean"
    ErrObj -> "ErrorType"
    FloatObj -> "Float"
    IntegerObj -> "Integer"
    NullObj -> "Null"
    ReturnObj -> "ReturnObject"
    StrObj -> "String"
  }
}

// ------------------------------------------------------------------------------------------------
// Environment
// ------------------------------------------------------------------------------------------------

pub type Environment {
  Environment(store: Dict(String, Object))
}

// ------------------------------------------------------------------------------------------------

pub fn new_environment() -> Environment {
  Environment(dict.new())
}

// ------------------------------------------------------------------------------------------------

pub fn get(env: Environment, name: String) -> #(Object, Bool) {
  case dict.get(env.store, name) {
    Ok(val) -> #(val, True)
    _ -> #(ObjNull(Null("null")), False)
  }
}

// ------------------------------------------------------------------------------------------------

pub fn set(
  env: Environment,
  name: String,
  object: Object,
) -> #(Environment, Object) {
  let new_env = Environment(dict.insert(env.store, name, object))
  let #(obj, _) = get(new_env, name)
  #(new_env, obj)
}
// ------------------------------------------------------------------------------------------------
