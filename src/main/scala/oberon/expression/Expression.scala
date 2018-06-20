package oberon.expression

trait Expression {
  def eval(): Value 
}

trait Value extends Expression {
  def eval() = this 
}

case class Undefined() extends Value
case class Uninitialized() extends Value
case class IntValue(value: Integer) extends Value
case class BoolValue(value: Boolean) extends Value
case class Return(value: Value) extends Value