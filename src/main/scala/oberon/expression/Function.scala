package oberon.expression

import oberon.command.Command
import oberon.defEnvironment._

case class Function(id: String, blockcmd: Command, args: List[Variable] = List()) extends defTrait{

  def declare(): Unit = {
    map(id, this)
  }

  def verify(param: List[Value]): Boolean = true

  def load_args(param: List[Value]): Unit = {
    for(i <- args.indices) {
     // new Assignment(args(i).id, param(i)).run()
    }
  }

  def eval(): Value = {
    val return_variable = Undefined()
    val list = List(1,2,3)
    print(list.tail)
    Return(return_variable).eval()
  }
}
