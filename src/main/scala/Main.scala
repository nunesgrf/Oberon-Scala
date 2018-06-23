import oberon.command._
import oberon.visitor._
import oberon.expression._
import oberon.expression.NotExpression

object Main extends App {
  val test = new PrettyPrinter
  val blockCommand = new Assignment("a",IntValue(2))
  val cond = new EqExpression(IntValue(2),IntValue(2))

  val decprocedure = DecProcedure("foo",blockCommand,List())
  test.visit(ProcedureCall("procedure",List(IntValue(2),IntValue(3))))
  new DecVar("Bool","Maravilha", BoolValue(true)).run()
  val ok = VarRef("Maravilha")

  test.visit(ok)

  println(test.str)
}