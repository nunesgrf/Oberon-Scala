package oberon.visitor

import oberon.InvalidArgument
import oberon.command.{IfThenElse, Return, _}
import oberon.expression.{BoolValue, NotExpression}
class Refact {

  def refactor(f: IfThenElse): Return = {
    f.ifCommand match {
      case Return(BoolValue(true))  => Return(f.cond)
      case Return(BoolValue(false)) => Return(NotExpression(f.cond))
    }
  }

  def visit(f: IfThenElse): Return = {
    f.ifCommand match {
      case Return(BoolValue(_)) => refactor(f)
      case _                    => throw InvalidArgument("Não é um IfThenElse refatorável")
    }
  }
}
