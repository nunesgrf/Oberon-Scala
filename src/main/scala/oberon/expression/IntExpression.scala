package oberon.expression

import oberon.visitor.Visitor

class AddExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs,rhs) {

  def eval(): Value = {
    val v1 = lhs.eval().asInstanceOf[IntValue]
    val v2 = rhs.eval().asInstanceOf[IntValue]

    IntValue(v1.value + v2.value)
  }

  def accept(v : Visitor) {
    v.visit(this)
  }
}

class SubExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs,rhs) {

  def eval(): Value = {
    val v1 = lhs.eval().asInstanceOf[IntValue]
    val v2 = rhs.eval().asInstanceOf[IntValue]

    IntValue(v1.value - v2.value)
  }

  def accept(v : Visitor) {
    v.visit(this)
  }
}

class DivExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs,rhs) {

  def eval(): Value = {
    val v1 = lhs.eval().asInstanceOf[IntValue]
    val v2 = rhs.eval().asInstanceOf[IntValue]

    IntValue(v1.value / v2.value)
  }

  def accept(v : Visitor) {
    v.visit(this)
  }
}

class MulExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs,rhs) {

  def eval(): Value = {
    val v1 = lhs.eval().asInstanceOf[IntValue]
    val v2 = rhs.eval().asInstanceOf[IntValue]

    IntValue(v1.value * v2.value)
  }

  def accept(v : Visitor) {
    v.visit(this)
  }
}

class ModExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs,rhs) {

  def eval(): Value = {
    val v1 = lhs.eval().asInstanceOf[IntValue]
    val v2 = rhs.eval().asInstanceOf[IntValue]

    IntValue(v1.value % v2.value)
  }

  def accept(v : Visitor) {
    v.visit(this)
  }
}

class EqExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs,rhs) {

  def eval(): Value = {
    val v1 = lhs.eval().asInstanceOf[IntValue]
    val v2 = rhs.eval().asInstanceOf[IntValue]

    BoolValue(v1.value == v2.value)
  }

  def accept(v : Visitor) {
    v.visit(this)
  }
}

class DifExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs,rhs) {

  def eval(): Value = {
    val v1 = lhs.eval().asInstanceOf[IntValue]
    val v2 = rhs.eval().asInstanceOf[IntValue]

    BoolValue(v1.value != v2.value)
  }

  def accept(v : Visitor) {
    v.visit(this)
  }
}

class MenExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs,rhs) {

  def eval(): Value = {
    val v1 = lhs.eval().asInstanceOf[IntValue]
    val v2 = rhs.eval().asInstanceOf[IntValue]

    BoolValue(v1.value < v2.value)
  }

  def accept(v : Visitor) {
    v.visit(this)
  }
}

class MaiExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs,rhs) {

  def eval(): Value = {
    val v1 = lhs.eval().asInstanceOf[IntValue]
    val v2 = rhs.eval().asInstanceOf[IntValue]

    BoolValue(v1.value > v2.value)
  }

  def accept(v : Visitor) {
    v.visit(this)
  }
}

class LEqExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs,rhs) {

  def eval(): Value = {
    val v1 = lhs.eval().asInstanceOf[IntValue]
    val v2 = rhs.eval().asInstanceOf[IntValue]

    BoolValue(v1.value <= v2.value)
  }

  def accept(v : Visitor) {
    v.visit(this)
  }
}

class MEqExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs,rhs) {

  def eval(): Value = {
    val v1 = lhs.eval().asInstanceOf[IntValue]
    val v2 = rhs.eval().asInstanceOf[IntValue]

    BoolValue(v1.value >= v2.value)
  }

  def accept(v : Visitor) {
    v.visit(this)
  }
}