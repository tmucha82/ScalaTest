package com.sdc.scala

import org.scalatest.FunSuite

class ListTest extends FunSuite {
  test("initialization of list") {
    val fruit = "apples" :: "oranges" :: "pears" :: Nil //the same as: List("apples", "oranges", "pears")
    val numbers = 1 :: 2 :: 3 :: 4 :: Nil
    val list = (1 :: 0 :: 0 :: Nil) ::
      (0 :: 1 :: 0 :: Nil) ::
      (0 :: 0 :: 1 :: Nil) :: Nil
    val empty = Nil

    assert(List("apples", "oranges", "pears") === fruit)
    assert(List(1, 2, 3, 4) === numbers)
    assert(List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1)) === list)
  }

  test("some method of list") {
    val fruits = "apples" :: "oranges" :: "pears" :: Nil //the same as: List("apples", "oranges", "pears")
    assert(false === fruits.isEmpty)
    assert("apples" === fruits.head)
    assert("oranges" :: "pears" :: Nil === fruits.tail)
    assert("oranges" === fruits.tail.head)
    intercept[NoSuchElementException] {
      Nil.head
    }
  }

  test("list patterns") {
    val fruits = "apples" :: "oranges" :: "pears" :: Nil //the same as: List("apples", "oranges", "pears")
    val List(a, b, c) = fruits
    assert("apples" === a)
    assert("oranges" === b)
    assert("pears" === c)

    val e :: f :: rest = fruits
    assert("apples" === e)
    assert("oranges" === f)
  }

  test("concatenating lists") {
    assert(List(1, 2, 3, 4) === List(1, 2) ::: List(3, 4))
    assert(List(1, 2, 3, 4, 5, 6) === List(1, 2) ::: List(3, 4) ::: List(5, 6))
  }

  test("append lists by hand using pattern matching") {
    def append[T](left: List[T], right: List[T]): List[T] = {
      left match {
        case List() => right
        case a :: b => a :: append(b, right)
      }
    }

    assert(List(1, 2, 3, 4) === append(List(1, 2), List(3, 4)))
    assert(List(1, 2, 3, 4, 5, 6) === append(List(1, 2), append(List(3, 4), List(5, 6))))
  }
}
