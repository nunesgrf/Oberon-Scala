package oberon.expression

import oberon.command.BlockCommand

abstract class Module(cmds: BlockCommand) {

}


class Procedure(cmds: BlockCommand) extends Module(cmds) {
  def exe = cmds.run()
}

class Return(exp: Value) extends Expression {

  def eval: Value = exp
}

class Function(cmds: BlockCommand) extends Module(cmds) {
  def exe: Value = {
    new Return(IntValue(5)).eval
  }
}