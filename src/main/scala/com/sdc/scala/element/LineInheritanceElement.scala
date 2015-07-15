package com.sdc.scala.element

private class LineInheritanceElement(content: String) extends ArrayElement(Array(content)) {
  override def demo() {
    println("LineInheritanceElement: demo")
  }
}
