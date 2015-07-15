package com.sdc.scala.element

private class LineCompositionElement(content: String) extends Element {

  override def contents: Array[String] = Array(content)

  override def demo() {
    println("LineCompositionElement: demo")
  }
}
