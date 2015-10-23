package com.sdc.scala.expressions

case class BinaryOperator(operator: String, right: Expression, left: Expression) extends Expression