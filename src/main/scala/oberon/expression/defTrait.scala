package oberon.expression
import oberon.command._

trait defTrait {

  val id: String
  val blockcmd: Command
  val args: List[Variable]

  def declare()
}
