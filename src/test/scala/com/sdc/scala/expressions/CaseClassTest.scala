package com.sdc.scala.expressions

import org.scalatest.FunSuite

class CaseClassTest extends FunSuite {

  test("factory of case class of expression") {
    //you could write like this
    var variable = new Variable("x")
    assert(variable.name === "x")

    //or like this
    variable = Variable("y")
    assert(variable.name === "y")
  }

  test("factory of operator") {
    val operator = BinaryOperator("+", Number(1), Variable("x"))
    assert(operator.operator === "+")
    assert(operator.right === Number(1))
    assert(operator.left === Variable("x"))
  }

  test("simplify using case classes and pattern matching") {
    assert(Expression.simplifyTop(UnaryOperator("-", UnaryOperator("-", Variable("x")))) === Variable("x"))
    assert(Expression.simplifyTop(BinaryOperator("+", Number(0), Variable("a"))) === Variable("a"))
    assert(Expression.simplifyTop(BinaryOperator("+", Variable("b"), Number(0))) === Variable("b"))
    assert(Expression.simplifyTop(BinaryOperator("*", Number(1), Variable("c"))) === Variable("c"))
    assert(Expression.simplifyTop(BinaryOperator("*", Variable("d"), Number(1))) === Variable("d"))
  }
}
