package com.sdc.scala.abstraction

import org.scalatest.FunSuite

class ConcreteTest extends FunSuite {

  test("simple concrete class") {
    class ConcreteString extends Abstract {
      type T = String

      def transform(x: String) = x + x

      val initial = "hi"
      var current = initial
    }

    val abstractClass: Abstract = new ConcreteString
    assert("hi" === abstractClass.initial)
    assert("hi" === abstractClass.current)
    assert("vivi" === abstractClass.transform("vi".asInstanceOf[abstractClass.T]))

  }

  test("abstract trait") {
    val abstractClass: Abstract = new Concrete
    assert(abstractClass === abstractClass.initial)
    assert(abstractClass === abstractClass.current)
    assert(abstractClass === abstractClass.transform(abstractClass.asInstanceOf[abstractClass.T]))
  }

  test("abstract violation") {
    abstract class Fruit {
      var test: String
      val v: String

      // `v' for value
      def m: String // `m' for method
    }

    abstract class Apple extends Fruit {
      var test: String
      val v: String
      val m: String // !! OK to override a `def' with a `val'
    }

    abstract class BadApple extends Fruit {
      var test: String

      //      def v: String // ERROR: cannot override a `val' with a `def'
      def m: String
    }

    class TestFruit extends Fruit {
      // could be like this
      //var test: String = "test"

      //or like these
      def test: String = "test"
      def test_=(x: String) = println(x)
      def m: String = "m"
      val v: String = "v"
    }
  }
}
