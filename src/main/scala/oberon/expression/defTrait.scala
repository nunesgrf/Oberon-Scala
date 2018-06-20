package oberon.expression
import oberon.command._

trait defTrait {

  val id: String
  val blockcmd: Command
  val args: List[Variable]

  def declare(): Unit
  def verify(param: List[Value]): Boolean
  def load_args(param: List[Value]): Unit
}
