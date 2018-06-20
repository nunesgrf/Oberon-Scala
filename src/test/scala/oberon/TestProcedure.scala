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

/*class TestProcedure extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  it should "salvar uma procedure quando a mesma é declarada" in {

    val values = List(IntValue(3), BoolValue(true))
    val procedure =  DecProcedure("foo", new BlockCommand(List()), values)

    procedure.declare()

    var result = ""
    lookup("foo") match {
      case None    => result = "Error"
      case Some(a) => result = a.id
    }

    result should be ("foo")
  }

  it should "print" in {

    val values = List(IntValue(3), BoolValue(true))
    val procedure =  DecProcedure("foo", new BlockCommand(List()), values)

    procedure.declare()

    val test = ProcedureCall("foo",List(IntValue(2),IntValue(3)))
    test.run()
  }

}
*/