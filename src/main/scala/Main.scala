import oberon.command._
import oberon.visitor._
import oberon.expression._
import oberon.expression.NotExpression

object Main extends App {
  val test = new PrettyPrinter

  val cond = new EqExpression(IntValue(2),IntValue(2))
  val list = List(new Assignment("a",IntValue(3)),new Assignment("b",IntValue(4)))
  val ifcommand = new BlockCommand(list)
  val elsecommand = new BlockCommand(list)

  val ifThenElse = new IfThenElse(cond,ifcommand,elsecommand)

  test.visit(ifThenElse)
  println(test.str)

  println("--------------------------------------------------")
  val While = new While(cond,ifThenElse)
  test.visit(While)
  println(test.str)
  println("--------------------------------------------------")

  val refact = new Refact
  val tste = new IfThenElse(new EqExpression(IntValue(3),IntValue(3)), Return(BoolValue(true)),Return(BoolValue(false)))
  test.visit(tste)
  println(test.str)
  refact.visit(tste)
  test.visit(refact.result)
  println(test.str)

  println("--------------------------------------------------")

  val tste3 = new Assignment("Teste",new AddExpression(IntValue(3),IntValue(5)))
  test.visit(tste3)
  println(test.str)

  refact.visit(tste3)
  test.visit(refact.result)
  println(test.str)


}