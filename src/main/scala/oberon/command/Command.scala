package oberon.command

import oberon.Environment._
import oberon.expression._

trait Command {
  def run() : Unit 
}

class BlockCommand(val cmds: List[Command]) extends Command {

  override
  def run() : Unit = {
    cmds.foreach(c => c.run())
  }
}

class Assignment(val id: String, val expression: Expression) extends Command {

  override
  def run() : Unit = {
    map(id, expression.eval())
  }

}

class While(val cond: Expression, val command: Command) extends Command {
  override
  def run() : Unit = {
    val v = cond.eval.asInstanceOf[BoolValue]

    v match {
      case BoolValue(true) => { command.run(); run(); }
      case _               => { } 
    }
  }
}

class Print(val exp: Expression) extends Command {
  override
  def run() : Unit = {
    println(exp.eval())
  }

}

class Conditional(val cond: Expression, val ifCommand: Command, val elseCommand: Command = new BlockCommand(List())) extends Command {

  override def run(): Unit = {

    val value = cond.eval.asInstanceOf[BoolValue]

    value match {
      case BoolValue(true) => ifCommand.run()
      case BoolValue(false) => elseCommand.run()
    }
  }
}

class For(val i: Expression, val range: Expression, val command: Command = new BlockCommand(List())) extends Command {

  override def run(): Unit = {

    val runLoop = new EqExpression(i,range)

    runLoop.eval match {
      case BoolValue(false) => {
        command.run()
        new For(new AddExpression(i,IntValue(1)), range, command).run()
      }
      case _ =>
    }
  }
}


