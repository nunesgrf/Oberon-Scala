package oberon.expression

import oberon.command.Command
import oberon.defEnvironment._
import oberon.expression._

case class Function(id: String, blockcmd: Command, args: List[Variable] = List()) extends defTrait{

  def declare(): Unit = {
    map(id, this)
  }

  def eval(): Value = {
    val return_variable = Undefined()
    
    Return(return_variable).eval()
  }
}
