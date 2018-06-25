package oberon.expression

import oberon.visitor.{Visitable, Visitor}

sealed trait Type

case class TBool() extends Type
case class TInt() extends Type
case class TUndefined() extends Type
case class TUninitialized() extends Type

trait Expression extends Visitable{
  def eval(): Value
  def calculateType() : Type
  def typeCheck() : Boolean = calculateType() != TUndefined()
}

trait Value extends Expression {
  def eval() = this 
}

case class Undefined() extends Value {

  override def calculateType() : Type = TUndefined()

  def accept(v : Visitor) {
    v.visit(this)
  }
}

case class Uninitialized() extends Value {

  override def calculateType() : Type = TUninitialized()

  def accept(v : Visitor) {
    v.visit(this)
  }
}

case class IntValue(value: Integer) extends Value {
  def accept(v : Visitor) {
    v.visit(this)
  }

  override def calculateType() : Type = TInt()
}

case class BoolValue(value: Boolean) extends Value {
  def accept(v : Visitor) {
    v.visit(this)
  }

  override def calculateType() : Type = TBool()
}