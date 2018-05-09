package oberon.expression

class BoolExpression(lhs: Expression, rhs: Expression = BoolValue(true)) extends BinExpression(lhs,rhs) {

  private val v1 = lhs.eval().asInstanceOf[BoolValue]
  private val v2 = rhs.eval().asInstanceOf[BoolValue]

  override def eval(): Value = BoolValue(true)
  def or: Value = BoolValue(v1.value || v2.value)
  def and: Value = BoolValue(v1.value && v2.value)
  def not: Value = BoolValue(!v1.value)
  def implies: Value = BoolValue(!v1.value || v2.value)
}
