package com.sdc.scala.element

abstract class Element {
  def contents: Array[String]

  def height: Int = contents.length

  def width: Int = if (height != 0) contents(0).length else 0

  def above(that: Element): Element = {
    new ArrayElement(this.contents ++ that.contents)
  }

  def beside(that: Element): Element = {
    val contents = new Array[String](this.contents.length)
    for (i <- 0 until this.contents.length)
      contents(i) = this.contents(i) + that.contents(i)
    new ArrayElement(contents)
  }

  def demo() {
    println("Element: demo")
  }
}
