package oberon.expression
import oberon.expression._
import oberon.command._
case class Procedure(val id : String, val blockcmd : Command = new BlockCommand(List()), paramtrs : List[Variable]) {
  

}
