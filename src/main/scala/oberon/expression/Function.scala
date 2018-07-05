package oberon.expression

import oberon.command.{Assignment, BlockCommand, Command}
import oberon.defEnvironment._
import oberon.InvalidArgument
import oberon.visitor.Visitor


case class DecFunction(typeFunct : String,id: String, blockcmd : BlockCommand = new BlockCommand(List()), args: List[Variable] = List()) extends defTrait {
  private var flag = true
  var returnVarieble = new Assignment(id, Uninitialized()) // VARIAVEL DE RETORNO SEMPRE TERA NO NOME DA FUNCAO
  this.returnVerify()

  def declare(): Unit = map(id, this)

  def verify(param: List[Value]): Boolean = {
    if (args.length == param.length) {
      for (i <- args.indices) {
        if (!args(i).isValueType(param(i))) flag = false
      }
      flag
    }
    else false
  }

  def load_args(param: List[Value]): Unit = {
    for (i <- args.indices) args(i).Assign(param(i))
  }

  def returnVerify(): Unit = { // Ultimo comando do bloco de comandos deve ser um Assingment para variavel de retorno
    blockcmd.run()
    var returnAux = oberon.Environment.lookup(id)
    returnAux match {
      case Some(IntValue(_)) => {
        typeFunct match {
          case "Int" =>
          case _ => throw InvalidArgument("Wrong return, waiting for a Boll")
        }
      }
       case Some(BoolValue(_)) =>{
          typeFunct match{
            case "Bool" =>
            case  _ => throw InvalidArgument("Wrong return, waiting for a Int")
          }
      }
      case _ => throw InvalidArgument("No return")
    }
  }
}

case class CallFunction(id : String, param : List[Value]) extends Command {
    private def exe(function : defTrait): Unit ={
      function.load_args(param)
      function.blockcmd.run()
    }
    def run(): Unit ={
      lookup(id) match {
        case None => throw InvalidArgument("Funcao inexistente")
        case Some(function) =>{
          if(function.verify(param)) this.exe(function)
          else throw InvalidArgument("Argumentos não condizem com a funcao")
        }
      }
    }
  def accept(v : Visitor) {
    v.visit(this)
  }

  def tc(): Boolean = true
}