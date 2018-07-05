package oberon.visitor

import oberon.{InvalidArgument, OberonProgram}
import oberon.command._
import oberon.expression._
class Refact extends Visitor {

  var result: Command = Return(IntValue(0))

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

  def visit(f: Return)        : Unit = { }
  def visit(f: CallFunction)  : Unit = { }
  def visit(f: ProcedureCall) : Unit = { }
  def visit(f: For)           : Unit = { }
  def visit(f: While)         : Unit = { }
  def visit(f: DecVar)        : Unit = { }
  def visit(f: BlockCommand)  : Unit = { }
  def visit(f: NotExpression) : Unit = { }
  def visit(f: Print)         : Unit = { }
  def visit(f: AndExpression) : Unit = { }
  def visit(f: OrExpression)  : Unit = { }
  def visit(f: MEqExpression) : Unit = { }
  def visit(f: LEqExpression) : Unit = { }
  def visit(f: MaiExpression) : Unit = { }
  def visit(f: DifExpression) : Unit = { }
  def visit(f: EqExpression)  : Unit = { }
  def visit(f: ModExpression) : Unit = { }
  def visit(f: DivExpression) : Unit = { }
  def visit(f: MulExpression) : Unit = { }
  def visit(f: SubExpression) : Unit = { }
  def visit(f: AddExpression) : Unit = { }
  def visit(f: BoolValue)     : Unit = { }
  def visit(f: IntValue)      : Unit = { }
  def visit(f: Undefined)     : Unit = { }
  def visit(f: Uninitialized) : Unit = { }
  def visit(f: MenExpression) : Unit = { }
  def visit(f: IfThen)        : Unit = { }
  def visit(f: VarRef)        : Unit = { }
  def visit(f: OberonProgram) : Unit = { }
}
