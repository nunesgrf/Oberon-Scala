package oberon.expression
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter
import oberon.expression._
import oberon.Environment._
import oberon.expression.Uninitialized
class TestVariable extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  behavior of "a Variable"

  it should "x have to be a variable uninitialized" in{
    var v1 =  Variable("Int", "x")
    lookup("x") should be (Some(Uninitialized()))
  }

  it should "y have to be a variable int value == 5" in{
    var v1 = Variable("Int", "y",IntValue(5))
    lookup("y") should be (Some(IntValue(5)))
  }

  it should "x have to be a variable bool value == true " in{
    var v1 = Variable("Bool", "x",BoolValue(true))
    lookup("x") should be (Some(BoolValue(true)))
  }
}
