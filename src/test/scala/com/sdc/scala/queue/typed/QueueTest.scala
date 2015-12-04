package com.sdc.scala.queue.typed

import org.scalatest.FunSuite

class QueueTest extends FunSuite {

  test("queue") {
    val q = Queue(1, 2, 3)
    assert(1 === q.head)
    assert(2 === q.tail.head)
    assert(3 === q.tail.tail.head)

  }

  test("non-variant type") {
    class Cell[T](init: T) {
      private[this] var current = init

      def get = current

      def set(x: T) {
        current = x
      }
    }

    val c1 = new Cell[String]("abc")
    val c2: Cell[String] = c1 // String and only string
    c2.set("1")
    assert("1" === c1.get)
  }

  test("covariant type") {
    class Cell[+T](init: T) {
      private[this] var current = init

      def get = current
      def set[U >: T](x: U) { current = x.asInstanceOf[T] } //+T - you cannot use contravariant method such set..
    }

    val c1 = new Cell[String]("abc")
    val c2: Cell[Any] = c1
    c2.set(1)
//    assert("1" === c1.get)
  }

  test("contravariant type") {
    class Cell[-T](init: T) {
      private[this] var current = init

      def get[U <: T]: U = throw new IllegalArgumentException //-T - you cannot use covariant method such get..
      def set(x: T) {
        current = x
      }
    }

    val c1 = new Cell[Any]("abc")
    val c2: Cell[String] = c1 // String and only string
    c2.set("1")
  }

  test("asInstanceOf method for non-covariant method") {
    val array = Array("1", "2")
    //    var genericArray: Array[Any] = array //cannot do that!!, but sometimes you need that so you could go like...
    val genericArray: Array[Any] = array.asInstanceOf[Array[Any]]
    assert(Array[String]("1", "2") === array)
    assert(Array[Any]("1", "2") === genericArray)
  }

  test("contra- and co- variance in details") {
    class GParent
    class Parent extends GParent
    class Child extends Parent

    class Box[+A] {
      def set[AA >: A](a: AA) = println(a)
    }
    class Box2[-A] {
      def get[AA <: A](): AA = throw new IllegalArgumentException
    }

    def foo(x: Box[Parent]): Box[Parent] = identity(x)
    def bar(x: Box2[Parent]): Box2[Parent] = identity(x)

    val boxWithChild = new Box[Child]
    foo(boxWithChild) // success
    boxWithChild.set(new Child)
    boxWithChild.set(new GParent)
    //    foo(new Box[GParent]) // type error

    //    bar(new Box2[Child]) // type error
    val boxWithGParent = new Box2[GParent]
    bar(boxWithGParent) // success
  }
}
