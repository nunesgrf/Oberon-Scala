package oberon.expression
import oberon.command._
import oberon.expression._
case class Variable(val variableType : String,val name : String, val value : Value = Uninitialized()){
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

