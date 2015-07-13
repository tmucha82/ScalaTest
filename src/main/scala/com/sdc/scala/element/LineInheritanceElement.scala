package com.sdc.scala.element

class LineInheritanceElement(content: String) extends ArrayElement(Array(content)) {
  override def demo() {
    println("LineInheritanceElement: demo")
  }
}
