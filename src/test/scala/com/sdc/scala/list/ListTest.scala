package com.sdc.scala.list

import org.scalatest.FunSuite

import scala.collection.mutable.ListBuffer

class ListTest extends FunSuite {

  test("obvious things about list") {
    // List is abstract - cannot be initialize by constructor
    //new List

    //List is covariant so e.g.: you can assign a value of type List[Int], say, to a variable of type List[Any]
    val list: List[Int] = List(1, 2, 3)
    val genericList: List[Any] = list
    assert(List.apply[Int](1, 2, 3) === list)
    assert(List.apply[Any](1, 2, 3) === genericList)
  }

  test(":: method of List") {
    abstract class Fruit
    case class Apple() extends Fruit
    case class Orange() extends Fruit

    val apples = new Apple :: Nil
    val fruits = new Orange :: apples
    assert(List.apply[Apple](new Apple) === apples)
    assert(List.apply[Fruit](new Orange, new Apple) === fruits)
  }

  test("increment every element of list") {
    /**
     * Therefore each recursive call requires a new stack frame.
     * On today's virtual machines this means
     * that you cannot apply incrementAll to lists of much more
     * than about 30,000 to 50,000 elements
     */
    def incrementAll(xs: List[Int]): List[Int] = xs match {
      case List() => List()
      case x :: xs1 => x + 1 :: incrementAll(xs1)
    }

    def incrementAllInefficient(xs: List[Int]): List[Int] = {
      var result = List[Int]() // a very inefficient approach
      for (x <- xs) result = result ::: List(x + 1)
      result
    }

    /**
     * A better alternative is to use a list buffer.
     * List buffers let you accumulate the elements of a list.
     */
    def incrementAllWithBuffer(xs: List[Int]): List[Int] = {
      val buf = new ListBuffer[Int]
      for (x <- xs) buf += x + 1
      buf.toList
    }

    def incrementAllMine(xs: List[Int]): List[Int] = {
      for (x <- xs) yield x + 1
    }

    def incrementAllWithMap(xs: List[Int]): List[Int] = {
      // xs.map((x: Int) => x + 1)
      xs.map(_ + 1)
    }

    val list = List(1, 2, 3, 4)
    assert(List(2, 3, 4, 5) === incrementAll(list))
    assert(List(2, 3, 4, 5) === incrementAllInefficient(list))
    assert(List(2, 3, 4, 5) === incrementAllWithBuffer(list))
    assert(List(2, 3, 4, 5) === incrementAllMine(list))
    assert(List(2, 3, 4, 5) === incrementAllWithMap(list))

  }
}
