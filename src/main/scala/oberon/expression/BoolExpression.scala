package oberon.expression

import oberon.visitor.Visitor


class OrExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs,rhs) {

  def eval(): BoolValue = {
    val v1 = lhs.eval().asInstanceOf[BoolValue]
    val v2 = rhs.eval().asInstanceOf[BoolValue]

    BoolValue(v1.value || v2.value)
  }

  def accept(v : Visitor) {
    v.visit(this)
  }

  def calculateType() : Type = {
    val t1 = lhs.calculateType()
    val t2 = rhs.calculateType()

    if(t1 == TBool() && t2 == TBool()) TBool()
    else TUndefined()
  }
}

class AndExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs,rhs) {

  def eval(): BoolValue = {
    val v1 = lhs.eval().asInstanceOf[BoolValue]
    val v2 = rhs.eval().asInstanceOf[BoolValue]

    BoolValue(v1.value && v2.value)
  }

  def accept(v : Visitor) {
    v.visit(this)
  }

  def calculateType() : Type = {
    val t1 = lhs.calculateType()
    val t2 = rhs.calculateType()

    if(t1 == TBool() && t2 == TBool()) TBool()
    else TUndefined()
  }
}

case class NotExpression(v: Expression) extends Expression {

  def eval(): BoolValue = {
    val v1 = v.eval().asInstanceOf[BoolValue]

    BoolValue(!v1.value)
  }

  def accept(v : Visitor) {
    v.visit(this)
  }

  def calculateType() : Type = v.calculateType()
}