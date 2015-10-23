package com.sdc.scala.rectangular

import org.scalatest.FunSuite

class RectangularTest extends FunSuite {
  test("rectangular shape") {
    val rectangle = new Rectangle(new Point(1, 1), new Point(10, 10))

    assert(rectangle.left === 1)
    assert(rectangle.right === 10)
    assert(rectangle.width === 9)
  }
}
