package oberon.expression

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter

class TestNotExpression extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  it should "return true for an input: false" in {
    val not = new BoolExpression(BoolValue(false)).not

    not.eval should be (BoolValue(true))
  }

  it should "return false for an input: true" in {
    val not = new BoolExpression(BoolValue(true)).not

    not.eval should be (BoolValue(false))
  }
}
