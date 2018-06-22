package oberon.expression

import oberon.Environment.lookup
import oberon.visitor.Visitor

class VarRef(id: String) extends Expression {

  def eval(): Value = {

    lookup(id) match {
      case Some(a) => a
      case _       => Undefined()
    }
  }

  def accept(v : Visitor) {
    v.visit(this)
  }
}

