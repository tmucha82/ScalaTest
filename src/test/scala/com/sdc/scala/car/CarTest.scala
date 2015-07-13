package com.sdc.scala.car

import org.scalatest.FunSuite

class CarTest extends FunSuite {

  test("for with double filter") {
    val cars = new Car("red", true) :: new Car("blue", false) :: Nil
    var result: Set[Car] = Set()
    for (car <- cars
         if car.valid
         if car.color == "red") {
      result += car
    }
    assert(1 === result.size)
    for (test <- result) {
      assert(true === test.valid)
      assert("red" === test.color)
    }
  }
}
