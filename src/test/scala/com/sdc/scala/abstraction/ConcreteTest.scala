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

      // def v: String // ERROR: cannot override a `val' with a `def'
      def m: String
    }

    class TestFruit extends Fruit {
      // could be like this
      //var test: String = "test"

      //or like this
      def test: String = "test"

      def test_=(x: String) = println(x)

      def m: String = "m"

      val v: String = "v"
    }
  }

  test("abstract values in tait") {
    trait RationalTrait {
      val numberArg: Int
      val denominatorArg: Int

      require(denominatorArg != 0)
      private val g = gcd(numberArg, denominatorArg)
      val number = numberArg / g
      val denominator = denominatorArg / g

      private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

      override def toString = number + "/" + denominator
    }

    //This example demonstrates that initialization order is not the same for class parameters and abstract fields
    // in this example it would require(denominatorArg != 0) throw exception because abstract values are evaluated after initialization of anonymous trait !!
    // it is different than for class parameters
    intercept[IllegalArgumentException] {
      new RationalTrait {
        override val denominatorArg: Int = 1
        override val numberArg: Int = 2
      }
    }

    // ... unless we do
    //1. Pre-initialized fields: Object with trait :)
    new {
      override val numberArg: Int = 1
      override val denominatorArg: Int = 2
      //      val test = this.numberArg //won't compile
      val test = numberArg //it is OK
    } with RationalTrait

    object TwoThirds extends {
      override val numberArg: Int = 2
      override val denominatorArg: Int = 3
    } with RationalTrait

    //
    new RationalTrait {
      lazy override val denominatorArg: Int = 1
      lazy override val numberArg: Int = 2
    }

    // finally...
    trait LazyRationalTrait {
      val numberArg: Int
      val denominatorArg: Int

      private val g = gcd(numberArg, denominatorArg)
      lazy val number = numberArg / g
      lazy val denominator = denominatorArg / g

      private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

      override def toString = number + "/" + denominator
    }
    new LazyRationalTrait {
      override val denominatorArg: Int = 1
      override val numberArg: Int = 2
    }
  }

  test("abstract types") {
    class Food
    abstract class Animal {
      type SuitableFood <: Food

      def eat(food: SuitableFood)
    }
    class Grass extends Food
    class Cow extends Animal {
      override type SuitableFood = Grass

      override def eat(food: Grass) = println()
    }
    class Fish extends Food

    class DogFood extends Food
    class Dog extends Animal {
      type SuitableFood = DogFood

      override def eat(food: DogFood) {}
    }

    val bessy = new Cow
    bessy.eat(new Grass)
    //    bessy.eat(new Fish)
    //    bessy.eat(new Food)

    val lassie = new Dog
    val anotherDog = new Dog
    lassie.eat(new DogFood)
//    lassie.eat(new bessy.SuitableFood) //won't compile
    lassie.eat(new anotherDog.SuitableFood) //this would

    //strange it is like above..but it does not work like you think it would
    {
      class Food
      abstract class Animal {
        def eat[T <: Food](food: T)
      }
      class Grass extends Food
      class Cow extends Animal {
        override def eat[Grass](food: Grass) {}
      }
      class Fish extends Food
      val bessy = new Cow
      bessy.eat(new Grass)
      bessy.eat(new Fish) //why?!
      bessy.eat(new Food) //why?!
    }
  }
}
