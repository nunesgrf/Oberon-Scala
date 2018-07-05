package oberon.expression
import oberon.InvalidArgument
import oberon.expression._
import oberon.command._
import oberon.defEnvironment._
import oberon.visitor.Visitor

case class DecProcedure(id : String, blockcmd : Command = new BlockCommand(List()), args: List[Variable] ) extends defTrait {

  private var flag = true

  def declare(): Unit = map(id,this)

  def verify(param: List[Value]): Boolean = {

    if(args.length == param.length) {
      for(i <- args.indices) {
        if(!args(i).isValueType(param(i))) flag = false
      }
      flag
    }
    else false
  }

  def load_args(param: List[Value]): Unit = {
    for(i <- args.indices) args(i).Assign(param(i))
  }

}

case class ProcedureCall(id: String, param: List[Value]) extends Command with ProcedureTrait {

  private def exe(procedure: defTrait): Unit = {
    procedure.load_args(param)
    procedure.blockcmd.run()
  }

  def run(): Unit = {
    lookup(id) match {
      case None            =>  throw InvalidArgument("Não existe tal procedimento")
      case Some(procedure) =>
        if(procedure.verify(param)) this.exe(procedure)
        else throw InvalidArgument("Argumentos não condizem com o procedimento")
    }
  }

  def accept(v : Visitor) {
    v.visit(this)
  }

  def tc(): Boolean = true

}