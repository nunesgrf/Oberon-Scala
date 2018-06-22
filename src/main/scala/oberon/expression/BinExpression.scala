package oberon.expression

abstract case class BinExpression(lhs: Expression, rhs: Expression) extends Expression {

}