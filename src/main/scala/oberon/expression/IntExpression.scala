package oberon.expression

class IntExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs,rhs) {

  private val v1 = lhs.eval().asInstanceOf[IntValue]
  private val v2 = rhs.eval().asInstanceOf[IntValue]

  override def eval(): Value = BoolValue(true)
  def mod: Value = IntValue(v1.value%v2.value)
  def div: Value = IntValue(v1.value/v2.value)
  def mul: Value = IntValue(v1.value*v2.value)
  def sub: Value = IntValue(v1.value-v2.value)
  def add: Value = IntValue(v1.value+v2.value)
  def eqq: Value = BoolValue(v1.value == v2.value)
  def dif: Value = BoolValue(v1.value != v2.value)
  def men: Value = BoolValue(v1.value < v2.value)
  def mai: Value = BoolValue(v1.value > v2.value)
  def meneqq: Value = BoolValue(v1.value < v2.value || v1.value == v2.value)
  def maieqq: Value = BoolValue(v1.value > v2.value || v1.value == v2.value)
}