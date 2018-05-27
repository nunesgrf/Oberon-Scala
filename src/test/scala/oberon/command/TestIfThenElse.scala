package oberon.command
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter


import oberon.Environment._
import oberon.expression.IntValue
import oberon.expression._
class TestIfThenElse extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {
  behavior of "a IfThenElse commmand"

  it should "if int value 20 == intvalue 50 then x -> 20, else x -> 100" in{
    val listIf : List[Command] =  List(new Assignment("x",IntValue(20)))
    val listElse : List[Command] = List(new Assignment("x",IntValue(100)))
    val cond = new IntExpression(IntValue(20),IntValue(50)).eqq
    val ifcmd = new BlockCommand(listIf)
    val elsecmd = new BlockCommand(listElse)
    val ifThenElse = new  IfThenElse(cond, ifcmd, elsecmd)
    ifThenElse.run()
    lookup("x") should be (Some(IntValue(100)))
  }
}
