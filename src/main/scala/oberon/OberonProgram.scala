package oberon

import oberon.command.Command
import oberon.visitor.Visitor

class OberonProgram(val cmd: Command) extends Command {

  override
  def run() : Unit = {
    cmd.run()
  }

  def accept(v : Visitor) {
    v.visit(this)
  }

  def tc(): Boolean = true
}
