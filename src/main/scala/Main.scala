import oberon.command.{Assignment, BlockCommand, Command, While}
import oberon.expression._
import oberon.Environment.lookup
object Main extends App {
  println("Hello, World!")

  val read = new ReadBoolean
  val op = new Assignment("a", read.eval )

  op.run
  println(lookup("a"))

  val list1: List[Command] = new Assignment("b",IntValue(3))::Nil
  val list = new BlockCommand(list1)
  new Procedure(list).exe

  println(lookup("b"))


  val ope = new Function(list)
  println(ope.exe)
}