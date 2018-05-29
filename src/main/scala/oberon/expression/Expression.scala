package oberon.expression


trait Expression {
  def eval(): Value 
}


trait Value extends Expression {
  def eval(): Value = this
}

case class Undefined() extends Value
case class IntValue(value: Integer) extends Value
case class BoolValue(value: Boolean) extends Value

class ReadInt() extends Expression {

  override def eval(): Value = IntValue(scala.io.StdIn.readInt())
}

class ReadBoolean() extends Expression {

  override def eval(): Value = BoolValue(scala.io.StdIn.readBoolean())

}

