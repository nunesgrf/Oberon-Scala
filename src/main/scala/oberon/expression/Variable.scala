package oberon.expression
import oberon.InvalidArgument
import oberon.command._
import oberon.visitor.Visitor

case class Variable(var variableType : String = "Undefined", name : String, var value : Value = Uninitialized()) extends Expression {

  Assign(this.value)

  def eval(): Value = this.value

  def Assign(v: Value): Variable = {

    v match {

      case Undefined()     =>
        if(variableType == "Undefined") this.value = v
        else throw InvalidArgument("Atribuição de Undefined em Defined")

      case IntValue(_)     =>
        if(variableType == "Undefined") this.variableType = "Int"
        if(variableType == "Int") this.value = v
        else throw InvalidArgument("Atribuição de Int em Boolean")

      case BoolValue(_)    =>
        if(variableType == "Undefined") this.variableType = "Bool"
        if(variableType == "Bool") this.value = v
        else throw InvalidArgument("Atribuição de Bool em Int")

      case Uninitialized() =>

    }
    new Assignment(name,value).run()
    this
  }

  override def calculateType(): Type = {
    value match {
      case IntValue(_) => TInt()
      case Uninitialized() => TUninitialized()
      case BoolValue(_)  => TBool()
      case _ => TUndefined()
    }
  }

  def accept(v: Visitor): Unit = {
    v.visit(this)
  }

  override def typeCheck(): Boolean = calculateType() != TUndefined()

  def isValueType(that: Value): Boolean = this.isItSameTypeOf(Variable("Undefined","toCompare").Assign(that))

  def isItSameTypeOf(that: Variable): Boolean = this.variableType == that.variableType
}

