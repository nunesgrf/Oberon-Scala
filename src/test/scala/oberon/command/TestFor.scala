package oberon.command

import oberon.expression._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter

import oberon.Environment._

class TestFor extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter{

  behavior of "a for"

  it should "" in {

    val x = new Assignment("x", IntValue(0))
    val loopCms = new BlockCommand(List(new Print(IntValue(1)), new Print(IntValue(2)), new Assignment("x", IntValue(2))))
    val loop = new For(IntValue(0),IntValue(2), loopCms)

    loop.run()

    lookup("x") should not be Some(IntValue(0))
  }
}
