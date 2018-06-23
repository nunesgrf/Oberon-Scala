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
import oberon.command._


class PrettyPrinter extends Visitor {
  var str = ""

  // Private definitions
  private def visitExp(e: Expression)         : String = {
    e.accept(this)
    val string = str

    string
  }

  private def visitBinExp(e: BinExpression) : (String, String) = {
    e.lhs.accept(this)
    val l = str

    e.rhs.accept(this)
    val r = str

    (l, r)
  }

  // Values
  def visit(e: Undefined)     : Unit = { str = "Undefined" }
  def visit(e: IntValue)      : Unit = { str = e.value.toString }
  def visit(e: BoolValue)     : Unit = { str = e.value.toString }
  def visit(e: Return)        : Unit = { str = e.value.toString }
  def visit(e: Uninitialized) : Unit = { str = "Uninitialized"  }

  // IntExpression
  def visit(e: AddExpression) : Unit = {
    val (l, r) = visitBinExp(e)
    str = "(" + l + " + " + r + ")"
  }
  def visit(e: LEqExpression) : Unit = {
    val (l, r) = visitBinExp(e)
    str = "(" + l + " <= " + r + ")"
  }
  def visit(e: EqExpression)  : Unit = {
    val (l, r) = visitBinExp(e)
    str = "(" + l + " == " + r + ")"
  }
  def visit(e: MEqExpression) : Unit = {
    val (l, r) = visitBinExp(e)
    str = "(" + l + " >= " + r + ")"
  }
  def visit(e: MenExpression) : Unit = {
    val (l, r) = visitBinExp(e)
    str = "(" + l + " < " + r + ")"
  }
  def visit(e: MaiExpression) : Unit = {
    val (l, r) = visitBinExp(e)
    str = "(" + l + " > " + r + ")"
  }
  def visit(e: DifExpression) : Unit = {
    val (l, r) = visitBinExp(e)
    str = "(" + l + " != " + r + ")"
  }
  def visit(e: ModExpression) : Unit = {
    val (l, r) = visitBinExp(e)
    str = "(" + l + " % " + r + ")"
  }
  def visit(e: DivExpression) : Unit = {
    val (l, r) = visitBinExp(e)
    str = "(" + l + " / " + r + ")"
  }
  def visit(e: MulExpression) : Unit = {
    val (l, r) = visitBinExp(e)
    str = "(" + l + " * " + r + ")"
  }
  def visit(e: SubExpression) : Unit = {
    val (l, r) = visitBinExp(e)
    str = "(" + l + " - " + r + ")"
  }

  // BoolExpression
  def visit(e: NotExpression) : Unit = {
    val a = visitExp(e.v)
    str = "!" + a
  }
  def visit(e: AndExpression) : Unit = {
    val (l, r) = visitBinExp(e)
    str = "(" + l + " && " + r + ")"
  }
  def visit(e: OrExpression)  : Unit = {
    val (l, r) = visitBinExp(e)
    str = "(" + l + " || " + r + ")"
  }

  // Commands
  def visit(e: VarRef)        : Unit = { }
  def visit(c: BlockCommand)  : Unit = { }
  def visit(c: Assignment)    : Unit = { }
  def visit(c: While)         : Unit = { }
  def visit(c: Print)         : Unit = { }
  def visit(c: OberonProgram) : Unit = { }
  def visit(c: IfThen)        : Unit = {
    val cond = visitExp(c.cond)

    str = "if(" + cond + "):\n \t" +
            c.command
  }
  def visit(c: IfThenElse)    : Unit = {

    val cond = visitExp(c.cond)

    str = "if(" + cond + "):\n \t" +
              c.ifCommand + "\n" +
          "else:\n \t" +
              c.elseCommand
  }
}