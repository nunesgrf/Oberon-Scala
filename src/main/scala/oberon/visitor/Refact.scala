package oberon.visitor

import oberon.{InvalidArgument, OberonProgram}
import oberon.command._
import oberon.expression._
class Refact extends Visitor {

  var result: Command = Return(IntValue(0))
  var expression: Expression = IntValue(0)

  private def refactor(c: Command): Command = {
    c.accept(this)
    result
  }

  private def refactor(f: IfThenElse): Return = {
      f.ifCommand match {
        case Return(BoolValue(true))  => Return(f.cond)
        case Return(BoolValue(false)) => Return(NotExpression(f.cond))
      }
    }

  private def refactor(f: Expression): Expression = f.eval()

  def visit(f: IfThenElse): Unit = {
    f.ifCommand match {
      case Return(BoolValue(_)) => result = refactor(f)
      case _                    => throw InvalidArgument("Não é um IfThenElse refatorável")
    }
  }

  def visit(f: Assignment): Unit =
    f.expression match {
      case BinExpression(_,_) => result = new Assignment(f.id,refactor(f.expression))
      case _                  => throw InvalidArgument("Não é uma BinExpression")
    }

  def visit(f: Return)        : Unit = {
    result = Return(refactor(f.value))
  }
  def visit(f: CallFunction)  : Unit = {
    result = f
  }
  def visit(f: ProcedureCall) : Unit = {
    result = f
  }
  def visit(f: For)           : Unit = {
    result = new For(refactor(f.i),refactor(f.range),refactor(f.command))
  }
  def visit(f: While)         : Unit = {
    result = new While(refactor(f.cond),refactor(f.command))
  }
  def visit(f: DecVar)        : Unit = {
    result = f
  }
  def visit(f: BlockCommand)  : Unit = {
    f.cmds.foreach(a => refactor(a))
  }
  def visit(f: NotExpression) : Unit = {
    expression = NotExpression(refactor(f.v))
  }
  def visit(f: Print)         : Unit = {
    result = new Print(refactor(f.exp))
  }
  def visit(f: AndExpression) : Unit = {
    expression = refactor(f)
  }
  def visit(f: OrExpression)  : Unit = {
    expression = refactor(f)
  }
  def visit(f: MEqExpression) : Unit = {
    expression = refactor(f)
  }
  def visit(f: LEqExpression) : Unit = {
    expression = refactor(f)
  }
  def visit(f: MaiExpression) : Unit = {
    expression = refactor(f)
  }
  def visit(f: DifExpression) : Unit = {
    expression = refactor(f)
  }
  def visit(f: EqExpression)  : Unit = {
    expression = refactor(f)
  }
  def visit(f: ModExpression) : Unit = {
    expression = refactor(f)
  }
  def visit(f: DivExpression) : Unit = {
    expression = refactor(f)
  }
  def visit(f: MulExpression) : Unit = {
    expression = refactor(f)
  }
  def visit(f: SubExpression) : Unit = {
    expression = refactor(f)
  }
  def visit(f: AddExpression) : Unit = {
    expression = refactor(f)
  }
  def visit(f: BoolValue)     : Unit = {
    expression = refactor(f)
  }
  def visit(f: IntValue)      : Unit = {
    expression = refactor(f)
  }
  def visit(f: Undefined)     : Unit = {
    expression = refactor(f)
  }
  def visit(f: Uninitialized) : Unit = {
    expression = refactor(f)
  }
  def visit(f: MenExpression) : Unit = {
    expression = refactor(f)
  }
  def visit(f: IfThen)        : Unit = {
    result = new IfThen(refactor(f.cond),refactor(f.command))
  }
  def visit(f: VarRef)        : Unit = {
    expression = VarRef(f.id).eval()
  }
  def visit(f: OberonProgram) : Unit = {
    f.cmd.accept(this)
  }
}
