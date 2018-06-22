package oberon.visitor

import oberon.expression.Undefined
import oberon.expression.IntValue
import oberon.expression.BoolValue
import oberon.expression.VarRef
import oberon.expression.BinExpression
import oberon.expression._

import oberon.command.BlockCommand
import oberon.command.Assignment
import oberon.command.While
import oberon.command.Print
import oberon.OberonProgram


class PrettyPrinter extends Visitor {
  var str = ""

  def visit(e: Undefined)     : Unit = { str = "Undefined" }

  def visit(e: IntValue)      : Unit = { str = e.value.toString }

  def visit(e: BoolValue)     : Unit = { str = e.value.toString }

  def visit(e: AddExpression) : Unit = {
    val (l, r) = visitBinExp(e)
    str = "(" + l + " + " + r + ")"
  }

  def visit(e: LEqExpression)  : Unit = {
    val (l, r) = visitBinExp(e)
    str = "(" + l + " <= " + r + ")"
  }

  def visit(e: EqExpression)  : Unit = {
    val (l, r) = visitBinExp(e)
    str = "(" + l + " == " + r + ")"
  }

  def visit(e: VarRef)        : Unit = { }
  def visit(c: BlockCommand)  : Unit = { }
  def visit(c: Assignment)    : Unit = { }
  def visit(c: While)         : Unit = { }
  def visit(c: Print)         : Unit = { }
  def visit(c: OberonProgram) : Unit = { }

  private def visitBinExp(e: BinExpression) : (String, String) = {
    e.lhs.accept(this)
    val l = str

    e.rhs.accept(this)
    val r = str

    (l, r)
  }
}