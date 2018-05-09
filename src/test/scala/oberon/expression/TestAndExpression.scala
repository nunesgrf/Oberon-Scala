package oberon.expression

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter

class TestAndExpression extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  it should "return false for an input: false && false" in {

    val and = new BoolExpression(BoolValue(false),BoolValue(false)).and

    and should be (BoolValue(false))
  }

  it should "return false for an input: true && false" in {

    val and = new BoolExpression(BoolValue(true),BoolValue(false)).and

    and should be (BoolValue(false))

  }

  it should "return false for an input: false && true" in {

    val and = new BoolExpression(BoolValue(false),BoolValue(true)).and

    and.eval() should be (BoolValue(false))
  }

  it should "return true for an input: true && true" in {

    val and = new BoolExpression(BoolValue(true),BoolValue(true)).and

    and.eval() should be (BoolValue(true))
  }
}
