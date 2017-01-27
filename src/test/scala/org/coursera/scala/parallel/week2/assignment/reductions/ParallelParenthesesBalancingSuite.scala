package org.coursera.scala.parallel.week2.assignment.reductions

import org.coursera.scala.parallel.week2.assignment.reductions.ParallelParenthesesBalancing._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  trait TestSet {
    def threshold = 2

    def checkSeq(input: String, expected: Boolean) = assert(balance(input.toArray) == expected, s"balance($input) should be $expected")

    def checkPar(input: String, expected: Boolean) = assert(parBalance(input.toArray, threshold) == expected, s"balance($input) should be $expected")

    def check(input: String, expected: Boolean) = {
      checkSeq(input, expected)
      checkPar(input, expected)
    }
  }

  test("balance and parBalance should work for empty string") {
    new TestSet {
      check("", expected = true)
    }
  }

  test("balance and parBalance should work for string of length 1") {
    new TestSet {
      check("(", expected = false)
      check(")", expected = false)
      check(".", expected = true)
    }
  }

  test("balance and parBalance should work for string of length 2") {
    new TestSet {
      check("()", expected = true)
      check(")(", expected = false)
      check("((", expected = false)
      check("))", expected = false)
      check(".)", expected = false)
      check(".(", expected = false)
      check("(.", expected = false)
      check(").", expected = false)
    }
  }

  test("balance and parBalance should work for large string") {
    new TestSet {
      override def threshold: Int = 3

      check("()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()", expected = true)
      check(")()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(", expected = false)
    }
  }
}