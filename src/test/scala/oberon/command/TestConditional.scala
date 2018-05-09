package oberon.command

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter


import oberon.Environment._
import oberon.expression.IntValue
import oberon.expression._

class TestConditional extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter{

  behavior of "a conditional command"

  it should "map x -> 10 when 1 == 1 and map x -> 20 when 1 != 1" in {

    val ifCms: List[Command] = List(new Assignment("x", IntValue(10)))
    val elseCms: List[Command] = List(new Assignment("x", IntValue(20)))

    val cond = new IntExpression(IntValue(1),IntValue(1)).eqq
    val ifCommand = new BlockCommand(ifCms)
    val elseCommand = new BlockCommand(elseCms)

    val conditional = new Conditional(cond,ifCommand,elseCommand)

    conditional.run()

    lookup("x") should be (Some(IntValue(10)))
  }
}
