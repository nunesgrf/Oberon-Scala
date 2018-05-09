package oberon.expression

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter

class TestOrExpression extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  it should "return false for an input: false && false" in {
    val or = new BoolExpression(BoolValue(false),BoolValue(false)).or

    or.eval should be (BoolValue(false))
  }
  it should "return true for an input: true && false" in {
    val or = new BoolExpression(BoolValue(true),BoolValue(false)).or

    or.eval should be (BoolValue(true))
  }
  it should "return true for an input: false && true" in {
    val or = new BoolExpression(BoolValue(false),BoolValue(true)).or

    or.eval should be (BoolValue(true))
  }
  it should "return true for an input: true && true" in {
    val or = new BoolExpression(BoolValue(true),BoolValue(true)).or

    or.eval should be (BoolValue(true))
  }
}
