package com.sdc.scala.conversions

import org.scalatest.FunSuite

import scala.language.implicitConversions
import scala.runtime.RichInt

class ConversionsTest extends FunSuite {


  def printWithSpaces(number: String) = number

  test("simple string conversion") {
    implicit def intWrapper(number: Int): String = {
      number.toString
    }

    val test = 4
    assert("4" === intWrapper(test))
    assert("4" === printWithSpaces(intWrapper(test)))
    assert("4" === printWithSpaces(test))
  }

  test("implicit conversion to an expected type") {
    implicit def doubleToInt(x: Double): Int = x.toInt

    val i: Int = 3.5 //compile because of above method
    assert(3 === i)
  }

  test("interoperating with new types") {
    class NewType(val value: Int) {
      def +(that: NewType): NewType = new NewType(this.value + that.value)

      def +(that: Int): NewType = new NewType(this.value + that)
    }

    object NewType {
      implicit def intToNewType(that: Int): NewType = new NewType(that)
    }

    assert(3 === (new NewType(1) + new NewType(2)).value)
    assert(3 === (new NewType(1) + 2).value)
    assert(3 === (2 + new NewType(1)).value)
  }

  test("implicit parameters") {
    class PreferredPrompt(val preference: String)
    class PreferredDrink(val preference: String)
    object Greeter {
      //this implicit is for all parameters
      def greet(name: String)(implicit prompt: PreferredPrompt, drink: PreferredDrink): String = "Welcome, " + name + ". Drink for you: " + drink.preference + ". " + prompt.preference
    }
    val bobsPrompt = new PreferredPrompt("relax> ")
    val bobsDrink = new PreferredDrink("coffee")
    assert("Welcome, Bob. Drink for you: coffee. relax> " === Greeter.greet("Bob")(bobsPrompt, bobsDrink))


    object JoePrefs {
      implicit val prompt = new PreferredPrompt("Yes, master> ")
      implicit val drink = new PreferredDrink("tea")
    }
    import JoePrefs._
    Greeter.greet("Joe")
    assert("Welcome, Joe. Drink for you: tea. Yes, master> " === Greeter.greet("Joe"))
  }

  test("implicit parameters one more time") {
    class OrderedInt(val value: Int) extends Ordered[OrderedInt] {
      override def compare(that: OrderedInt): Int = {
        if (this.value < that.value) -1
        else
        if (this.value > that.value) 1
        else
          0
      }
    }
    def maxListUpBound[T <: Ordered[T]](elements: List[T]): T =
      elements match {
        case List() => throw new IllegalArgumentException("empty list!")
        case List(x) => x
        case x :: rest =>
          val maxRest = maxListUpBound(rest)
          if (x > maxRest) x
          else maxRest
      }

    //Int is not ordered!!!! - that is why we use OrderedInt
    assert(5 === maxListUpBound[OrderedInt](List(new OrderedInt(3), new OrderedInt(5), new OrderedInt(2), new OrderedInt(1))).value)

    //... but we could use implicit !!!!
    def maxListImpParam[T](elements: List[T])(implicit ordered: T => Ordered[T]): T =
      elements match {
        case List() =>
          throw new IllegalArgumentException("empty list!")
        case List(x) => x
        case x :: rest =>
          val maxRest = maxListImpParam(rest)(ordered) // (ordered) is implicit - you could miss this !! - take a look below
          if (ordered(x) > maxRest) x
          else maxRest
      }
    assert(5 === maxListImpParam(List(3, 5, 2, 1)))
    assert(5 === maxListImpParam(List(3, 5, 2, 1))((x: Int) => new RichInt(x)))
  }

  test("view bounds - deprecated") {
    //    def maxList[T](elements: List[T])(implicit ordered: T => Ordered[T]): T =
    def maxList[T <% Ordered[T]](elements: List[T]): T =
      elements match {
        case List() =>
          throw new IllegalArgumentException("empty list!")
        case List(x) => x
        case x :: rest =>
          val maxRest = maxList(rest)
          if (x > maxRest) x
          else maxRest
      }
    // maxList[T](elements: List[T])(implicit ordered: T => Ordered[T]): T ==== maxList[T <% Ordered[T]](elements: List[T]): T
    // it means = I can use any T, so long as T can be treated as an Ordered[T].
    //UNFORTUNATELY VIEW BOUND ARE DEPRECATED
    assert(5 === maxList(List(3, 5, 2, 1)))
  }
}
