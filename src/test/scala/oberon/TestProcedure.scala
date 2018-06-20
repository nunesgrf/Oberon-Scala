package oberon

import oberon.command.{BlockCommand, While}
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter
import oberon.expression._
import oberon.expression.Variable
import oberon.defEnvironment._
import oberon.command._

class TestProcedure extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  it should "salvar uma procedure quando a mesma é declarada" in {

    val varTests = List(Variable("Bool","VarTest1"), Variable("Int","VarTest2"))
    val procedure =  DecProcedure("foo", new BlockCommand(List()), varTests)

    procedure.declare()

    var result = ""
    lookup("foo") match {
      case None    => result = "Error"
      case Some(a) => result = a.id
    }

    result should be ("foo")
  }

  it should "print" in {

    val varTests = List(Variable("Bool","VarTest1"), Variable("Int","VarTest2"))
    val procedure =  DecProcedure("foo", new BlockCommand(List()), varTests)

    procedure.declare()

    val test = ProcedureCall("foo",List(BoolValue(false),IntValue(3)))
    test.run()
  }

}