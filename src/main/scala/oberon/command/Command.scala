package oberon.command

import oberon.Environment._
import oberon.expression._
import oberon.visitor.{Visitable, Visitor}

trait Command extends Visitable{
  def run() : Unit
  def tc() : Boolean // a type checker for commands.
}

case class Return(value: Expression) extends Command {
  def run(): Unit = {}
  def accept(v: Visitor): Unit = {
    v.visit(this)
  }
  def tc(): Boolean = value.typeCheck()
}

class BlockCommand(val cmds: List[Command]) extends Command {

  override
  def run() : Unit = {
    cmds.foreach(c => c.run())
  }

  def accept(v : Visitor) {
    v.visit(this)
  }

  def tc(): Boolean = cmds.forall(c => c.tc())
}

class Assignment(val id: String, val expression: Expression) extends Command {

  override
  def run() : Unit = {


    map(id, expression.eval())
  }

  def accept(v : Visitor) {
    v.visit(this)
  }
  def tc(): Boolean = expression.typeCheck()
}

class While(val cond: Expression, val command: Command) extends Command {
  override
  def run() : Unit = {
    val v = cond.eval().asInstanceOf[BoolValue]

    v match {
      case BoolValue(true) => command.run(); run();
      case _               =>
    }
  }

  def accept(v : Visitor) {
    v.visit(this)
  }
  def tc(): Boolean = command.tc() && cond.calculateType() == TBool()
}

class Print(val exp: Expression) extends Command {
  override
  def run() : Unit = {
    println(exp.eval())
  }

  def accept(v : Visitor) {
    v.visit(this)
  }
  def tc(): Boolean = true
}

class For(val i: Expression, val range: Expression, val command: Command = new BlockCommand(List())) extends Command {

  override def run(): Unit = {

    val runLoop = new EqExpression(i,range)

    runLoop.eval() match {
      case BoolValue(false) => {
        command.run()
        new For(new AddExpression(i,IntValue(1)), range, command).run()
      }
      case _ =>
    }
  }

  def accept(v : Visitor) {
    v.visit(this)
  }

  def tc(): Boolean = i.calculateType() == TInt() && range.calculateType() == TInt() && command.tc()
}

class IfThen(val cond: Expression, val command : Command = new BlockCommand(List())) extends Command{
   override def run(): Unit ={
      val value = cond.eval().asInstanceOf[BoolValue]

      value match{
        case BoolValue(true)  => command.run()
        case BoolValue(false) =>
      }
  }

  def accept(v : Visitor) {
    v.visit(this)
  }

  def tc(): Boolean = cond.calculateType() == TBool() && command.tc()
}

class IfThenElse(val cond : Expression, val ifCommand : Command = new BlockCommand(List()), val elseCommand : Command= new BlockCommand(List())) extends Command {
  override def run(): Unit ={
    val value = cond.eval().asInstanceOf[BoolValue]

    value match{
      case BoolValue(true) => ifCommand.run()
      case BoolValue(false) => elseCommand.run()
    }
  }

  def accept(v : Visitor) {
    v.visit(this)
  }

  def tc(): Boolean = cond.calculateType() == TBool() && ifCommand.tc() && elseCommand.tc()
}

class DecVar(val datatype: String, val name: String, val value: Value = Uninitialized()) extends Command {
  def run(): Unit = {
    Variable(datatype,name,value)
  }

  def accept(v : Visitor) {
    v.visit(this)
  }
  def tc(): Boolean = true
}
