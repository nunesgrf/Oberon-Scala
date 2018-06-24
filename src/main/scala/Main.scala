import oberon.command._
import oberon.visitor._
import oberon.expression._
import oberon.expression.NotExpression

object Main extends App {
  val test = new PrettyPrinter

  val cond = new EqExpression(IntValue(2),IntValue(2))
  val ifelsecomand = new BlockCommand(List(new Print(IntValue(2)), new Print(IntValue(4))))
  val ifThenElse = new For(cond,IntValue(2),ifelsecomand)

  val whilecommand = new BlockCommand(List(ifThenElse))
  test.visit(ifThenElse)

  println(test.str)
}