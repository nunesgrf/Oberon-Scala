package oberon.expression

import oberon.Environment.lookup
import oberon.visitor.Visitor

case class VarRef(id: String) extends Expression {

  def eval(): Value = {

    lookup(id) match {
      case Some(a) => a
      case _       => Undefined()
    }
  }

  def accept(v : Visitor) {
    v.visit(this)
  }

  def calculateType() : Type = {

    lookup(id) match {
      case Some(a) => a.calculateType()
      case _       => TUndefined()
    }
  }
}

