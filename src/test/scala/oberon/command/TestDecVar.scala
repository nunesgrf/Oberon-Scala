package oberon.command

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter
import oberon.expression._
class TestDecVar extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  behavior of "Declaration of variables"

  it should "Create an IntValue(0) with name: variable" in {
    val test = new DecVar("int","variable")
    test.run()
    new VarRef("variable").eval() should be (Uninitialized())
  }
}
