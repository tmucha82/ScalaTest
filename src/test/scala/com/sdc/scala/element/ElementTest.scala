package com.sdc.scala.element

import org.scalatest.FunSuite

class ElementTest extends FunSuite {

  test("element construct") {
    val element = new Element {
      override def contents: Array[String] = Array("1", "22", "333")
    }
    assert(3 === element.height)
    assert(1 === element.width)
  }

}
