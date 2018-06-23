import oberon.command.IfThenElse
import oberon.visitor._
import oberon.expression._
import oberon.expression.NotExpression

object Main extends App {
  val test = new PrettyPrinter

  test.visit(new IfThenElse(new EqExpression(IntValue(2),IntValue(3))))
  println(test.str)
}
