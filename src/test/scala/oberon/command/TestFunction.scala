package oberon.command
import oberon.InvalidArgument
import oberon.command.BlockCommand
import oberon.defEnvironment._
import oberon.expression._
import org.scalatest.{BeforeAndAfter, FlatSpec, GivenWhenThen, Matchers}

class TestFunction extends FlatSpec with  Matchers with BeforeAndAfter  with  GivenWhenThen{
   behavior of "a function"
   it should "verificar se a funcao esta salva" in{
      var variables = List(Variable("Int","x"),Variable("Int","y"))
      var block = new BlockCommand(List(new Assignment("funct",IntValue(20))))
      var funct = DecFunction("Int","funct",block,variables)
      funct.declare()
      var result = ""
      lookup("funct") match{
        case None => result ="Error"
        case Some(a) => result = a.id
      }
     result should be ("funct")

   }
  it should "rodando a funçao" in{
    var param = List(Variable("Int","w"),Variable("Int","q"))
    var block = new BlockCommand(List(new Assignment("foo",IntValue(20))))
    var funct = DecFunction("Int","foo",block,param)
    funct.declare()

    var functCall = CallFunction("foo",List(IntValue(0),IntValue(5)))
    functCall.run()
  }
}
