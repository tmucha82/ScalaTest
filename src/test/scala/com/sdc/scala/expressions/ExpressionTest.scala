package com.sdc.scala.expressions

import org.scalatest.{FunSuite, PrivateMethodTester}

class ExpressionTest extends FunSuite with PrivateMethodTester {
  test("set of precedences") {

    val expressionFormatter = new ExpressionFormatter()
    val decoratedExpressionFormatter = PrivateMethod[Map[String, Int]]('precedence)
    val precedence = expressionFormatter invokePrivate decoratedExpressionFormatter()
    assert(6 === precedence.get("*").get)
    assert(6 === precedence.get("%").get)
    assert(5 === precedence.get("+").get)
    assert(5 === precedence.get("-").get)
    assert(4 === precedence.get("<").get)
    assert(4 === precedence.get("<=").get)
    assert(4 === precedence.get(">").get)
    assert(4 === precedence.get(">=").get)
    assert(3 === precedence.get("==").get)
    assert(3 === precedence.get("!=").get)
    assert(2 === precedence.get("^").get)
    assert(1 === precedence.get("&").get)
    assert(1 === precedence.get("&&").get)
    assert(0 === precedence.get("|").get)
    assert(0 === precedence.get("||").get)
  }

  test("to string of expression") {
    val expressionFormatter = new ExpressionFormatter

    //should be "((x + y) * z) + 1
    val expression1 =
      BinaryOperator("+",
        BinaryOperator("*",
          BinaryOperator("+", Variable("x"), Variable("y")),
          Variable("z")),
        Number(1))

    val expression2 = BinaryOperator("*", BinaryOperator("/", Number(1), Number(2)), BinaryOperator("+", Variable("x"), Number(1)))
    val expression3 = BinaryOperator("+", BinaryOperator("/", Variable("x"), Number(2)), BinaryOperator("/", Number(1.5), Variable("x")))
    val expression4 = BinaryOperator("/", expression2, expression3)

    println(expressionFormatter.format(expression1))
    println(expressionFormatter.format(expression2))
    println(expressionFormatter.format(expression3))
    println(expressionFormatter.format(expression4))
  }
}
