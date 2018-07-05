import oberon.command._
import oberon.visitor._
import oberon.expression._

object Main extends App {
  val test = new PrettyPrinter

  val cond = new EqExpression(IntValue(2),IntValue(2))
  val list = List(new Assignment("a",IntValue(3)),new Assignment("b",IntValue(4)))
  val ifcommand = new BlockCommand(list)
  val elsecommand = new BlockCommand(list)

  val ifThenElse = new IfThenElse(cond,ifcommand,elsecommand)

  test.visit(ifThenElse)
  println("--------------------- imprime IfThenElse ----------------------------")
  println(test.str)

  println("------------------Refatoramente IfThenElse --------------------------")

  val refact = new Refact

  val tste = new IfThenElse(new EqExpression(IntValue(3),IntValue(3)), Return(BoolValue(true)),Return(BoolValue(false)))

  println("Sem refatoramento: \n")
  test.visit(tste)
  println(test.str)
  println("\n")
  println("Com refatoramento: \n")
  refact.visit(tste)
  test.visit(refact.result)
  println(test.str)

  println("----------------------Refatoramento Assigment --------------------------")

  val tste3 = new Assignment("Teste",new AddExpression(IntValue(3),IntValue(5)))

  println("Sem refatoramento:")
  test.visit(tste3)
  println("\t" + test.str)

  println("Com refatoramento:")
  refact.visit(tste3)
  test.visit(refact.result)
  println("\t" + test.str)
  println("\n")


}