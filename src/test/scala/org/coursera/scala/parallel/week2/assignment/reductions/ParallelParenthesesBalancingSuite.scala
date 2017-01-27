package org.coursera.scala.parallel.week2.assignment.reductions

import org.coursera.scala.parallel.week2.assignment.reductions.ParallelParenthesesBalancing._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) = assert(balance(input.toArray) == expected, s"balance($input) should be $expected")

    check("", expected = true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) = assert(balance(input.toArray) == expected, s"balance($input) should be $expected")

    check("(", expected = false)
    check(")", expected = false)
    check(".", expected = true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) = assert(balance(input.toArray) == expected, s"balance($input) should be $expected")

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