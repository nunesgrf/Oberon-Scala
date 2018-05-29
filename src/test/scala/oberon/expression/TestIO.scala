package oberon.expression

import oberon.command.{Assignment, DecVar, Print}
import oberon.expression.ReadInt
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter

class TestIO extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  it should "read an input from user and print it" in {

    val op1 = new DecVar("int", "variable")
    op1.run()

    val op3 = new ReadInt

    val op2 = new Assignment("variable", op3.eval)



  }
}
