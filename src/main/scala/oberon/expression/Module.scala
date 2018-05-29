package oberon.expression

import oberon.command._

abstract class Module(cmds: List[Command]) {

}


class Procedure(cmds: List[Command]) extends Module(cmds) {
  def exe: Unit = {
    new BlockCommand(cmds).run()
  }
}

/*class Return(exp: Value) extends Expression {

  def eval: Value = exp
}*/

class Function(cmds: List[Command]) extends Module(cmds) {
  def exe: Value = {
    val test = cmds.last.getClass.toString
    test match {
      case "class oberon.command.Return" => {
        new BlockCommand(cmds).run()
      }
      case _ => println("Não é função")
    }
   new Return(IntValue(5)).eval
  }
}