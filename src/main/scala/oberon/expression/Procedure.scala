package oberon.expression
import oberon.expression._
import oberon.command._
import oberon.defEnvironment._

case class Procedure(id : String, blockcmd : Command = new BlockCommand(List()), args: List[Variable] ) extends ProcedureTrait with defTrait{

  def declare(): Unit = {
    map(id,this)
  }

  def run(): Unit = {
    blockcmd.run()
  }
}
