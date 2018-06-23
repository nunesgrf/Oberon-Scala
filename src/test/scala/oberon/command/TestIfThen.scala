package oberon.command
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter


import oberon.Environment._
import oberon.expression.IntValue
import oberon.expression._

class TestIfThen extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {
  behavior of "a IfThen command"

  it should "if 10==10, x is on top of stack" in {
    val listCommand: List[Command] = List(new Assignment("x", IntValue(20)))
    val cond = new EqExpression(IntValue(10), IntValue(10))
    val command = new BlockCommand(listCommand)
    val ifcommand = new IfThen(cond, command)
    ifcommand.run()
    lookup("x") should be(Some(IntValue(20)))
  }
  }
