package com.sdc.scala.abstraction

import org.scalatest.FunSuite

class EnumerationTest extends FunSuite {

  test("create simple enumeration") {
    object Color extends Enumeration {
      val Red = Value
      val Green = Value
      val Blue = Value
    }
    assert(Color.Blue === Color.Blue)
    assert(Color.Blue.id === Color.Blue.id)
    assert(0 === Color.Red.id)
    assert(1 === Color.Green.id)
    assert(2 === Color.Blue.id)
  }

  test("enum with values") {
    object Direction extends Enumeration {
      val North = Value("North")
      val East = Value("East")
      val South = Value("South")
      val West = Value("West")
    }
    for (d <- Direction.values) println(d)
    assert(Direction.North === Direction(0))
    assert(Direction.East === Direction(1))
    assert(Direction.South === Direction(2))
    assert(Direction.West === Direction(3))
  }
}
