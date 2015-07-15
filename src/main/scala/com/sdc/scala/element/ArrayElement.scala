package com.sdc.scala.element

private class ArrayElement(val contents: Array[String]) extends Element {
  override def demo() {
    println("ArrayElement: demo")
  }
}
