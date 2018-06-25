package oberon.expression

import oberon.command.BlockCommand
import oberon.defEnvironment._
import org.scalatest.{BeforeAndAfter, FlatSpec, GivenWhenThen, Matchers}

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

    val test = ProcedureCall("foo",List(BoolValue(true),IntValue(3)))
    test.run()
  }

}