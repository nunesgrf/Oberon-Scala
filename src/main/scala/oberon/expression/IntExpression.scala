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

  def calculateType() : Type = {
    val t1 = lhs.calculateType()
    val t2 = rhs.calculateType()

    if(t1 == TInt() && t2 == TInt()) TInt()
    else TUndefined()
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

  def calculateType() : Type = {
    val t1 = lhs.calculateType()
    val t2 = rhs.calculateType()

    if(t1 == TInt() && t2 == TInt()) TInt()
    else TUndefined()
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

  def calculateType() : Type = {
    val t1 = lhs.calculateType()
    val t2 = rhs.calculateType()

    if(t1 == TInt() && t2 == TInt()) TInt()
    else TUndefined()
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

  def calculateType() : Type = {
    val t1 = lhs.calculateType()
    val t2 = rhs.calculateType()

    if(t1 == TInt() && t2 == TInt()) TInt()
    else TUndefined()
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

  def calculateType() : Type = {
    val t1 = lhs.calculateType()
    val t2 = rhs.calculateType()

    if(t1 == TInt() && t2 == TInt()) TInt()
    else TUndefined()
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

  def calculateType() : Type = {
    val t1 = lhs.calculateType()
    val t2 = rhs.calculateType()

    if(t1 == t2) TBool()
    else TUndefined()
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

  def calculateType() : Type = {
    val t1 = lhs.calculateType()
    val t2 = rhs.calculateType()

    if(t1 == t2) TBool()
    else TUndefined()
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

  def calculateType() : Type = {
    val t1 = lhs.calculateType()
    val t2 = rhs.calculateType()

    if(t1 == TInt() && t2 == TInt()) TBool()
    else TUndefined()
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

  def calculateType() : Type = {
    val t1 = lhs.calculateType()
    val t2 = rhs.calculateType()

    if(t1 == TInt() && t2 == TInt()) TBool()
    else TUndefined()
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

  def calculateType() : Type = {
    val t1 = lhs.calculateType()
    val t2 = rhs.calculateType()

    if(t1 == TInt() && t2 == TInt()) TBool()
    else TUndefined()
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

  def calculateType() : Type = {
    val t1 = lhs.calculateType()
    val t2 = rhs.calculateType()

    if(t1 == TInt() && t2 == TInt()) TBool()
    else TUndefined()
  }
}