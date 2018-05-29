import oberon.command._
import oberon.expression._
import oberon.Environment.lookup
object Main extends App {
  println("Hello, World!")

  val read = new ReadBoolean
  val op = new Assignment("a", read.eval )

  op.run
  println(lookup("a"))

  val list1: List[Command] = new Assignment("b",IntValue(3))::new Return(IntValue(3))::Nil

  val ope = new Function(list1)
  println(ope.exe)

  println(lookup("b"))

}