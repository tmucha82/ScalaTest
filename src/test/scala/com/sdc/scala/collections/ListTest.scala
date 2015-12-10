package com.sdc.scala.collections

import org.scalatest.FunSuite

import scala.util.Random

class ListTest extends FunSuite {
  test("initialization of list") {
    val fruit = "apples" :: "oranges" :: "pears" :: Nil //the same as: List("apples", "oranges", "pears")
    val numbers = 1 :: 2 :: 3 :: 4 :: Nil
    val list = (1 :: 0 :: 0 :: Nil) ::
      (0 :: 1 :: 0 :: Nil) ::
      (0 :: 0 :: 1 :: Nil) :: Nil
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

    val e :: f :: _ = fruits
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

  test("displaying lists: toString and mkString") {
    val letters = List('a', 'b', 'c', 'd', 'e') // a b c d e
    assert("List(a, b, c, d, e)" === letters.toString)
    assert("abcde" === letters.mkString)
    assert("[a, b, c, d, e]" === letters.mkString("[", ", ", "]"))
    assert(letters.toString === letters.mkString("List(", ", ", ")"))

    val builder = new StringBuilder
    letters.addString(builder, "[", ", ", "]")
    assert("[a, b, c, d, e]" === builder.toString)
  }

  test("converting lists: elements, toArray, copyToArray") {
    val letters = List('a', 'b', 'c', 'd', 'e') // a b c d e
    val lettersArray = letters.toArray
    assert(Array('a', 'b', 'c', 'd', 'e') === lettersArray)
    assert(List('a', 'b', 'c', 'd', 'e') === lettersArray.toList)

    val array = new Array[Int](10)
    assert(Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0) === array)
    List(1, 2, 3) copyToArray(array, 3)
    assert(Array(0, 0, 0, 1, 2, 3, 0, 0, 0, 0) === array)

    val iterator = letters.iterator
    assert('a' === iterator.next)
    assert('b' === iterator.next)
  }

  test("mapping over lists: map, flatMap and foreach") {
    //map
    val list = List(1, 2, 3, 4)
    assert(List(2, 3, 4, 5) === list.map(_ + 1))
    assert(List(2, 3, 4, 5) === list.map((element: Int) => element + 1))
    assert(List(2, 3, 4, 5) === list.map(element => element + 1))

    val words = List("the", "quick", "brown", "fox")
    assert(List("eht", "kciuq", "nworb", "xof") === words.map(_.reverse))

    assert(List(List('t', 'h', 'e'), List('q', 'u', 'i', 'c', 'k'), List('b', 'r', 'o', 'w', 'n'), List('f', 'o', 'x')) === words.map(_.toList))
    assert(List('t', 'h', 'e', 'q', 'u', 'i', 'c', 'k', 'b', 'r', 'o', 'w', 'n', 'f', 'o', 'x') === words.flatMap(_.toList))
    assert(List('t', 'h', 'e', 'q', 'u', 'i', 'c', 'k', 'b', 'r', 'o', 'w', 'n', 'f', 'o', 'x') === words.map(_.toList).flatMap(_.toList))

    val range = List.range(1, 5)
    assert(List(1, 2, 3, 4) === range)

    //all possible pairs 1 ≤ j < i < 5: two ways: map and for
    assert(List((2, 1), (3, 1), (3, 2), (4, 1), (4, 2), (4, 3)) === range.flatMap((i: Int) => List.range(1, i).map(j => (i, j))))
    assert(List((2, 1), (3, 1), (3, 2), (4, 1), (4, 2), (4, 3)) === (for (i <- range; j <- List.range(1, i)) yield (i, j)))

    //foreach - sum of list
    var sum = 0
    list.foreach((element: Int) => sum += element)
    assert(10 === sum)

    sum = 0
    list.foreach(element => sum += element)
    assert(10 === sum)

    sum = 0
    list.foreach(sum += _)
    assert(10 === sum)
  }

  test("filtering lists: filter, partition, find, takeWhile, dropWhile, and span") {
    val list = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    // filter
    assert(list === list.filter((x: Int) => true))
    assert(Nil === list.filter((x: Int) => false))
    assert(List(2, 4, 6, 8, 10) === list.filter((x: Int) => x % 2 == 0))
    assert(List(2, 4, 6, 8, 10) === list.filter(x => x % 2 == 0))
    assert(List(2, 4, 6, 8, 10) === list.filter(_ % 2 == 0))

    // partition
    assert((List(2, 4, 6, 8, 10), List(1, 3, 5, 7, 9)) === list.partition(_ % 2 == 0))

    // find
    assert(2 === list.find(_ % 2 == 0).get)
    assert(None === list.find(_ < 0))

    // take while
    assert(List(1, 2) === list.takeWhile(_ < 3))
    assert(List(1, 2, 3) === list.take(3))

    // drop while
    assert(List(3, 4, 5, 6, 7, 8, 9, 10) === list.dropWhile(_ < 3))
    assert(List(4, 5, 6, 7, 8, 9, 10) === list.drop(3))

    val n = Random.nextInt(list.length)
    assert(list === list.take(n) ::: list.drop(n))

    // span
    assert((List(1, 2, 3, 4), List(5, 6, 7, 8, 9, 10)) === list.span(_ % 5 != 0))
  }

  test("predicates over lists: forall and exists") {
    val list = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    assert(list.forall(_ <= 10))
    assert(list.exists(_ % 9 == 0))
    assert(list.contains(9))
  }

  test("folding lists: /: and :\\") {
    val list = List(1, 2, 3, 4)

    //0 as first element, like in E in math: ((0 + list(0)) + list(1)) + ....
    def sum(list: List[Int]): Int = (0 /: list)(_ + _)
    //def sum(list: List[Int]): Int = (0 /: list)((s, element) => s + element) //like that also
    assert(10 === sum(list))

    //1 as first element, like in PI in math: ((1 * list(0)) * list(1)) * ....
    def product(list: List[Int]): Int = (1 /: list) {
      _ * _
    }
    assert(24 === product(list))

    val words = List("Hello", "World,", "Scala", "!!!")
    def concatenate(words: List[String]): String = (words.head /: words.tail)(_ + " " + _)
    assert("Hello World, Scala !!!" === concatenate(words))

    def reverse[T](list: List[T]): List[T] = (List[T]() /: list) { (x, y) => y :: x }
    assert(List(4, 3, 2, 1) === reverse(list))
  }

  test("sorting lists: sort") {
    val list = List(1, -3, 4, 2, 6)
    val words = List("the", "quick", "brown", "fox")

    assert(List(-3, 1, 2, 4, 6) === list.sorted)
    assert(List(-3, 1, 2, 4, 6) === list.sortWith((x, y) => x < y))
    assert(List(-3, 1, 2, 4, 6) === list.sortWith(_ < _))

    assert(List("brown", "fox", "quick", "the") === words.sorted)
    assert(List("the", "fox", "quick", "brown") === words.sortWith((x, y) => x.length < y.length))
    assert(List("the", "fox", "quick", "brown") === words.sortWith(_.length < _.length))
  }

  test("methods of the List object") {
    // Creating lists from their elements: List.apply
    assert(List(1, 2) === List.apply(1, 2))

    // Creating a range of numbers: List.range
    assert(List(1, 2, 3, 4) === List.range(1, 5))
    assert(List(2, 4, 6, 8) === List.range(2, 10, 2))

    // Creating uniform lists: List.make
    assert(List('a', 'a', 'a', 'a', 'a') === List.fill(5)('a'))
    assert(List("hello", "hello", "hello") === List.fill(3)("hello"))

    // Concatenating lists
    assert(List('a', 'b', 'c') === List.concat(List(), List('a', 'b'), List('c')))
  }
}
