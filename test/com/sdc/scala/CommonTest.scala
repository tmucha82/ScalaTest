package com.sdc.scala

import org.scalatest.FunSuite

class CommonTest extends FunSuite {
  def increase = {
    println("increase method")
    (x: Int) => x + 1
  }

  def increasePlaceholder = (_: Int) + 1

  def add = (_: Int) + (_: Int)

  test("simple increase function ") {
    assert(11 === increase(10))
  }

  test("simple increase placeholder function ") {
    assert(11 === increasePlaceholder(10))
  }

  test("simple for each") {
    val testList = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val result = testList.filter((x: Int) => x % 2 == 0)
    assert(List(2, 4, 6, 8) === result)
  }

  test("simple for each - short form") {
    val testList = List(-2, 24, 3, -2, -4, 5, 0)
    val result = testList.filter(x => x > 0)
    assert(List(24, 3, 5) === result)
  }

  test("placeholder simple test") {
    val testList = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val result = testList.filter(_ % 2 == 0)
    assert(List(0, 2, 4, 6, 8, 10) === result)
  }

  test("placeholder add test") {
    assert(7 === add(3, 4))
  }
  test("println list in different ways") {
    val testList = List(1, 2, 3, 4, 5)
    println(testList)
    testList foreach print
    testList.foreach(print)
    testList.foreach((x: Int) => print(x))
    testList.foreach(x => print(x))
    testList.foreach(print(_))
    for (x <- testList) print(x)
  }

  def sum(a: Int, b: Int, c: Int) = a + b + c

  test("partially applied functions") {
    val a = sum _
    assert(6 === sum(1, 2, 3))
    assert(6 === a(1, 2, 3))
    assert(6 === a.apply(1, 2, 3))


    val b = sum(1, _: Int, 3)
    assert(6 === b(2))
    assert(6 === b.apply(2))
  }

}
