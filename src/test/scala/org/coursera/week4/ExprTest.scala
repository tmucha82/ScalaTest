package org.coursera.week4

import org.scalatest.FunSuite

class ExprTest extends FunSuite {

  trait TestSet {
    val expr1 = Sum(Prod(Number(2), Var("x")), Var("y"))
    val expr2 = Prod(Sum(Number(2), Var("x")), Var("y"))
  }

  test("show") {
    new TestSet {
      println(""Expr.show(expr1))
      println(Expr.show(expr2))
    }
  }
}
