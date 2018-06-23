package oberon.expression

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter


class TestSubExpression extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  behavior of "an add expressions"

  it should "return value -5 in Sub(IntValue(5), IntValue(10))" in {
    val sub   = new SubExpression(IntValue(5), IntValue(10))
    sub.eval() should be (IntValue(-5))
  }

  it should "lead to an exception in Sub(IntValue(5), BoolValue(False))" in {
    //val add = new IntExpression(IntValue(5), BoolValue(false)).sub

    // add.eval() should be (IntValue(5))
  }
}
