package com.sdc.scala.expressions

abstract class Expression

object Expression {
  def simplifyAll(expression: Expression): Expression = {
    expression match {
      case UnaryOperator("-", UnaryOperator("-", exp)) => simplifyAll(exp)
      case BinaryOperator("+", exp, Number(0)) => simplifyAll(exp)
      case BinaryOperator("+", Number(0), exp) => simplifyAll(exp)
      case BinaryOperator("*", exp, Number(1)) => simplifyAll(exp)
      case BinaryOperator("*", Number(1), exp) => simplifyAll(exp)
      case UnaryOperator(operator, exp) => UnaryOperator(operator, simplifyAll(exp))
      case BinaryOperator(operator, right, left) => BinaryOperator(operator, simplifyAll(right), simplifyAll(left))
      case _ => expression
    }
  }
}