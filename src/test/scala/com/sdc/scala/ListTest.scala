package com.sdc.scala

import org.scalatest.FunSuite

import scala.util.Random

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

  test("init and last element of list") {
    val letters = List('a', 'b', 'c', 'd', 'e')

    //this is more expensive than head and tail, like list.isEmpty and list.length == 0
    assert('e' === letters.last)
    assert(List('a', 'b', 'c', 'd') === letters.init)

    //like head and tail
    assert('a' === letters.head)
    assert(List('b', 'c', 'd', 'e') === letters.tail)

    intercept[NoSuchElementException] {
      List().last
    }
    intercept[UnsupportedOperationException] {
      List().init
    }
    intercept[NoSuchElementException] {
      List().head
    }
    intercept[UnsupportedOperationException] {
      List().tail
    }
  }

  test("reverse of list") {
    val letters = List('a', 'b', 'c', 'd', 'e')
    assert(letters.reverse.reverse == letters)
    assert(letters.reverse.tail == letters.init.reverse) //d c b a
    assert(letters.reverse.init == letters.tail.reverse) //e d c b
    assert(letters.reverse.head === letters.last) //e
    assert(letters.reverse.last === letters.head) //a
  }

  test("own revers of list") {
    def reverse[T](list: List[T]): List[T] = list match {
      case List() => list
      case element :: subList => reverse(subList) ::: List(element)
    }
    val letters = List('a', 'b', 'c', 'd', 'e')
    println(reverse(letters))
  }

  test("prefixes and suffixes: drop, take and splitAt") {
    val letters = List('a', 'b', 'c', 'd', 'e')

    val n = new Random().nextInt(letters.length)
    assert(letters === letters.take(n) ::: letters.drop(n))

    //take - first n
    assert(List(letters.head) === letters.take(1))
    assert(letters.init === letters.take(letters.length - 1))

    //drop - first n
    assert(letters.tail === letters.drop(1))
    assert(List(letters.last) === letters.drop(letters.length - 1))

    //splitAt n - take n ::: drop n
    assert((List(letters.head), letters.tail) === letters.splitAt(1))
    assert((letters.init, List(letters.last)) === letters.splitAt(letters.length - 1))
  }

  test("element selection: apply and indices") {
    val letters = List('a', 'b', 'c', 'd', 'e')
    val n = new Random().nextInt(letters.length)

    //apply - like get
    assert('b' === letters.apply(1))
    assert(letters.last === letters.apply(letters.length - 1))
    assert(letters.drop(n).head === letters.apply(n))
    assert(List(0, 1, 2, 3, 4) === letters.indices) // a b c d e
  }

  test("zipping lists: zip") {
    val letters = List('a', 'b', 'c', 'd', 'e') // a b c d e
    val indexes = letters.indices //0 1 2 3 4

    assert(List(('a', 0), ('b', 1), ('c', 2), ('d', 3), ('e', 4)) === letters.zip(indexes))
    assert(List((0, 'a'), (1, 'b'), (2, 'c'), (3, 'd'), (4, 'e')) === indexes.zip(letters))
    assert(List(('a', 'a'), ('b', 'b'), ('c', 'c'), ('d', 'd'), ('e', 'e')) === letters.zip(letters))
    assert(List((0, 0), (1, 1), (2, 2), (3, 3), (4, 4)) === indexes.zip(indexes))
  }
}
