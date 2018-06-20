package oberon.expression
import oberon.command._

case class Variable(var variableType : String = "Undefined", name : String, var value : Value = Uninitialized()) extends Value {


  variableType match{

    case "int"  => var variable = new DecVar("int", name).run()
                   variable = new Assignment(name,value).run()

    case "bool" => var variable = new DecVar("bool", name).run()
                   variable = new Assignment(name, value).run()

    case _      => val variable  = Undefined()
  }

  override def eval(): Value = this.value

  def Assign(v: Value): Variable = {

    v match {

      case Undefined()  =>
        if(variableType == "Undefined") this.value = v
        else println("Atribuição de Undefined em Defined")

      case IntValue(_)  =>
        if(variableType == "Undefined") this.variableType = "Int"
        if(variableType == "Int") this.value = v
        else println("Atribuição de Int em Boolean")

      case BoolValue(_) =>
        if(variableType == "Undefined") this.variableType = "Bool"
        if(variableType == "Bool") this.value = v
        else println("Atribuição de Bool em Int")
    }
    this
  }

  def isValueType(that: Value): Boolean = this.isItSameTypeOf( Variable("Undefined","toCompare").Assign(that) )

  def isItSameTypeOf(that: Variable): Boolean = this.variableType == that.variableType
}

