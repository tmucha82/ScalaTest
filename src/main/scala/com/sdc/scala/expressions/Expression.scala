package com.sdc.scala.expressions

abstract class Expression

object Expression {
  def simplifyTop(expression: Expression): Expression = {
    expression match {
      case UnaryOperator("-", UnaryOperator("-", exp)) => exp
      case BinaryOperator("+", exp, Number(0)) => exp
      case BinaryOperator("+", Number(0), exp) => exp
      case BinaryOperator("*", exp, Number(1)) => exp
      case BinaryOperator("*", Number(1), exp) => exp
      case _ => expression
    }
  }
}