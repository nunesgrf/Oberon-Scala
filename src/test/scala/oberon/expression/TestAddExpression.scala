package oberon.expression

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter


class TestAddExpression extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  behavior of "an add expressions"

  it should "return value 15 in Add(IntValue(5), IntValue(10))" in {
    val op = new IntExpression(IntValue(5),IntValue(10)).add

    op should be (IntValue(15))
  }

  it should "lead to an exception in Add(IntValue(5), BoolValue(False))" in {
    val val5 = IntValue(5)
    val valf = BoolValue(false)
    val add = new IntExpression(val5, valf).add

    add.eval() should be (IntValue(5))
  }
}
