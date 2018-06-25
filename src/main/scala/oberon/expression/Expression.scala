package oberon.expression

import oberon.visitor.{Visitable, Visitor}

trait Expression extends Visitable{
  def eval(): Value 
}

trait Value extends Expression {
  def eval() = this 
}

case class Undefined() extends Value {

  def accept(v : Visitor) {
    v.visit(this)
  }
}
case class Uninitialized() extends Value {
  def accept(v : Visitor) {
    v.visit(this)
  }
}
case class IntValue(value: Integer) extends Value {
  def accept(v : Visitor) {
    v.visit(this)
  }
}
case class BoolValue(value: Boolean) extends Value {
  def accept(v : Visitor) {
    v.visit(this)
  }
}