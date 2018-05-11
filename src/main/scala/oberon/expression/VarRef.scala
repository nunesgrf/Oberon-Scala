package oberon.expression

import oberon.Environment.lookup

class VarRef(id: String) extends Expression {

  def eval: Value = {

    lookup(id) match {
      case Some(a) => a
      case _       => Undefined()
    }
  }
}
