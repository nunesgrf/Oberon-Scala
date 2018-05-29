package oberon.expression

import oberon.command.{BlockCommand, Command}

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

  override def eval(): Value = {
    println("Cheguei aqui")
    IntValue(scala.io.StdIn.readInt())
  }
}

class ReadBoolean() extends Expression {

  override def eval(): Value = {
    BoolValue(scala.io.StdIn.readBoolean())
  }
}

