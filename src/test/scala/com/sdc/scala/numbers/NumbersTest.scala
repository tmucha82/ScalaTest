package com.sdc.scala.numbers

import org.scalatest.FunSuite

class NumbersTest extends FunSuite {
  test("half of even number") {
    assert(5 === Numbers.half(10))
    assert(2 === Numbers.half(4))
    assert(7 === Numbers.half(14))
  }

  test("half of odd number with intercept") {
    intercept[IllegalArgumentException] {
      Numbers.half(9)
    }
  }

  test("half of odd number with catch") {
      try {
      Numbers.half(7)
      fail()
    } catch {
      case ex: IllegalArgumentException => //assert OK
      case ex: Exception => fail()
    }
  }

  test("take additional food") {
    assert("pepper" === pickAdditionalFood("salt"))
    assert("salsa" === pickAdditionalFood("chips"))
    assert("bacon" === pickAdditionalFood("eggs"))
    assert("hmm" === pickAdditionalFood("blah"))
  }

  def pickAdditionalFood(food: String): String = {
    val additionalFood = food match {
      case "salt" => "pepper"
      case "chips" => "salsa"
      case "eggs"  => "bacon"
      case _ => "hmm"
    }
    additionalFood
  }
}
