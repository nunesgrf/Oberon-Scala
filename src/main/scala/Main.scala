import oberon.visitor._
import oberon.expression._

object Main extends App {
  val test = new PrettyPrinter

  print(test.visit(IntValue(2)))
}