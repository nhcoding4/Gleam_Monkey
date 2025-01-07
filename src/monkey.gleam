import object
import repl

pub fn main() {
  repl.repl(object.new_environment())
}
