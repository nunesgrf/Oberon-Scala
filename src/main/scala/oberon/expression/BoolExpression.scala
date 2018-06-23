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
}

case class NotExpression(v: Expression) extends Expression {

  def eval(): BoolValue = {
    val v1 = v.eval().asInstanceOf[BoolValue]

    BoolValue(!v1.value)
  }

  def accept(v : Visitor) {
    v.visit(this)
  }
}