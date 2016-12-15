package org.coursera.scala.functional.week4

import org.scalatest.FunSuite

class NatTest extends FunSuite {

  trait TestSet {
    val zero = Zero
    val one = new Succ(Zero)
    val two = new Succ(one)
    val three = new Succ(two)
  }

  test("successor") {
    new TestSet {
      assert(true === (one == zero.successor))
      assert(true === (two == one.successor))
      assert(true === (three == two.successor))
    }
  }

  test("operator + ") {
    new TestSet {
      assert(true === (three == (one + two)))
      assert(true === (three == (one + two)))
    }

  }

  test("operator -") {
    new TestSet {
      assert(true === (zero == (three - three)))
      assert(true === (one == (three - two)))
      assert(true === (two == (three - one)))
    }

  }

  test("isZero") {
    new TestSet {
      assert(zero.isZero)
      assert(!one.isZero)
      assert(!two.isZero)
      assert(!three.isZero)
    }

  }

  test("predecessor") {
    new TestSet {
      assert(true === (zero == one.predecessor))
      assert(true === (one == two.predecessor))
      assert(true === (two == three.predecessor))
    }
  }

  test("operator ==") {
    new TestSet {
      assert(true === (zero == zero))
      assert(true === (one == one))
      assert(true === (two == two))
      assert(false === (one == two))
      assert(false === (two == one))
    }
  }
}
