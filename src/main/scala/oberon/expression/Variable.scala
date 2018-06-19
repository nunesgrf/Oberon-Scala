package oberon.expression
import oberon.command._

case class Variable(variableType : String, name : String, value : Value = Uninitialized()){
  variableType match{
    case "int" =>{ var variable = new DecVar("int", name).run()
                     variable = new Assignment(name,value).run()
    }
    case "bool" => {var variable = new DecVar("bool", name).run()
                    variable = new Assignment(name, value).run()
    }
    case _   =>  val variable  = Undefined()
  }

}

