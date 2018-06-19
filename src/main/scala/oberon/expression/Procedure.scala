package oberon.expression
import oberon.expression._
import oberon.command._
import oberon.Environment._

case class Procedure(id : String, blockcmd : Command = new BlockCommand(List()), paramtrs: List[Variable] ) {

  def declare(): Unit = {
    mapProcedure(id,this)
  }
}
