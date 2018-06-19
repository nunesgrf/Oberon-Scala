package oberon

import oberon.command.BlockCommand
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter
import oberon.expression.Procedure
import oberon.expression.Variable
import oberon.Environment._

class TestProcedure extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  it should "salvar uma procedure quando a mesma é declarada" in {

    val variable =  Variable("int","teste")
    val list = List(variable)
    val procedure =  Procedure("foo", new BlockCommand(List()),list)

    procedure.declare()

    var result = ""
    lookupProcedure("foo") match {
      case None    => result = "Error"
      case Some(a) => result = a.id
    }

    result should be ("foo")


  }

}
