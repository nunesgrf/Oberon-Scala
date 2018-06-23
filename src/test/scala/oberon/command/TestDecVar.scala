package oberon.command

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter
import oberon.expression._
class TestDecVar extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  behavior of "Declaration of variables"

  it should "Create an uninitialized variable named variable " in {
    val test = new DecVar("int","variable")
    test.run()
    VarRef("variable").eval() should be (Uninitialized())
  }
}
