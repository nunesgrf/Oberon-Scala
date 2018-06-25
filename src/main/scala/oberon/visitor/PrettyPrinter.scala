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
  private def visitExp(e: Expression)       : String          = {
    e.accept(this)
    val string = str

    string
  }
  private def visitBinExp(e: BinExpression) :(String, String) = {
    e.lhs.accept(this)
    val l = str

    e.rhs.accept(this)
    val r = str

    (l, r)
  }
  private def visitCommand(c: Command)      : String          = {
    c.accept(this)
    val string = str

    string
  }
  private def f(a: Command)                 : String          = {
    val string = "\t" + visitCommand(a) + "\n"
    string
  }
  private def TabCommand(s: String)         : String          = {
    val string = "\t" + s
    string
  }

  // Values
  def visit(e: Undefined)     : Unit = { str = "Undefined"      }
  def visit(e: IntValue)      : Unit = { str = e.value.toString }
  def visit(e: BoolValue)     : Unit = { str = e.value.toString }
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
  def visit(e: VarRef)        : Unit = {
    str = e.id
  }
  // TODO: BlockCommand com problemas
  def visit(c: BlockCommand)  : Unit = {
    str = ""
    c.cmds.foreach(a => str += f(a))
  }
  def visit(c: Assignment)    : Unit = {
    val a = visitExp(c.expression)

    str = c.id + " = " + a
  }
  def visit(c: While)         : Unit = {
    val cond = visitExp(c.cond)
    val command = visitCommand(c.command)

    str = "While(" + cond + "):" + "\n" +
          command
  }
  def visit(c: Print)         : Unit = {
    val a = visitExp(c.exp)

    str = "Print(" + a + ")"
  }
  def visit(c: IfThen)        : Unit = {
    val cond = visitExp(c.cond)
    val ifCommand = visitCommand(c.command)

    str = "if(" + cond + "):\n" +
              ifCommand
  }
  def visit(c: IfThenElse)    : Unit = {

    val cond = visitExp(c.cond)
    val ifCommand = visitCommand(c.ifCommand)

    val elseCommand = visitCommand(c.elseCommand)
    str = "if(" + cond + "):\n"+
              ifCommand + "\n" +
          "else:\n" +
              elseCommand
  }
  def visit(c: For)           : Unit = {
    val command = visitCommand(c.command)
    val i = visitExp(c.i)
    val range = visitExp(c.range)

    str = "For(i <- " + i + " to " + range + "): \n" + command


  }
  def visit(c: DecVar)        : Unit = {
    str = c.datatype + " " + c.name
  }
  def visit(c: ProcedureCall) : Unit = {

    val procedure_name = c.id
    str = procedure_name + "("

    for(i <- c.param.indices) {
      str += visitExp(c.param(i))
      if(i == c.param.length - 1) str += ")"
      else str += ","
    }
  }
  def visit(c: CallFunction) : Unit = {
    val functName = c.id
    str = functName + "("
    for(i <- c.param.indices){
      str+=  visitExp(c.param(i))
      if(i == c.param.length - 1) str += ")"
      else str += ","
    }
  }

  def visit(c: OberonProgram) : Unit = { }
}