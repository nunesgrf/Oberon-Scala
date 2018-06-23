package oberon.expression

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter

class TestAndExpression extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  it should "return false for an input: false && false" in {

    val and = new AndExpression(BoolValue(false),BoolValue(false)).eval()

    and should be (BoolValue(false))
  }

  it should "return false for an input: true && false" in {

    val and = new AndExpression(BoolValue(true),BoolValue(false)).eval()

    and should be (BoolValue(false))

  }

  it should "return false for an input: false && true" in {

    val and = new AndExpression(BoolValue(false),BoolValue(true)).eval()

    and.eval() should be (BoolValue(false))
  }

  it should "return true for an input: true && true" in {

    val and = new AndExpression(BoolValue(true),BoolValue(true)).eval()

    and.eval() should be (BoolValue(true))
  }
}
