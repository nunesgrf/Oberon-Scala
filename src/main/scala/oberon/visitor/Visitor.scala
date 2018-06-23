package oberon.visitor

import oberon.expression.Undefined
import oberon.expression.IntValue
import oberon.expression.BoolValue
import oberon.expression.VarRef
import oberon.expression._

import oberon.command.BlockCommand
import oberon.command.Assignment
import oberon.command.While
import oberon.command.Print
import oberon.command.IfThen
import oberon.command._
import oberon.OberonProgram

trait Visitable {
  def accept(v : Visitor) : Unit
}

trait Visitor {

  // Values
  def visit(e: Uninitialized) : Unit
  def visit(e: Undefined)     : Unit
  def visit(e: IntValue)      : Unit
  def visit(e: BoolValue)     : Unit
  def visit(e: VarRef)        : Unit
  def visit(e: Return)        : Unit

  // IntExpression
  def visit(e: AddExpression) : Unit
  def visit(e: SubExpression) : Unit
  def visit(e: MulExpression) : Unit
  def visit(e: DivExpression) : Unit
  def visit(e: ModExpression) : Unit
  def visit(e: EqExpression)  : Unit
  def visit(e: DifExpression) : Unit
  def visit(e: MaiExpression) : Unit
  def visit(e: MenExpression) : Unit
  def visit(e: LEqExpression) : Unit
  def visit(e: MEqExpression) : Unit

  // BoolExpression
  def visit(e: OrExpression)  : Unit
  def visit(e: AndExpression) : Unit
  def visit(e: NotExpression) : Unit

  // Commands
  def visit(c: BlockCommand)  : Unit
  def visit(c: Assignment)    : Unit
  def visit(c: While)         : Unit
  def visit(c: Print)         : Unit
  def visit(c: OberonProgram) : Unit
  def visit(c: IfThen)        : Unit
  def visit(c: IfThenElse)    : Unit
  def visit(c: DecVar)        : Unit
  def visit(c: For)           : Unit
  def visit(c: ProcedureCall) : Unit
}