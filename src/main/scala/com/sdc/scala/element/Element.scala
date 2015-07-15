package com.sdc.scala.element

import com.sdc.scala.element.Element.create

abstract class Element {
  def contents: Array[String]

  def height: Int = contents.length

  def width: Int = if (height != 0) contents(0).length else 0

  def above(that: Element): Element = {
    create(this.contents ++ that.contents)
  }

  def beside(that: Element): Element = {
    val contents = new Array[String](this.contents.length)
    for (i <- 0 until this.contents.length)
      contents(i) = this.contents(i) + that.contents(i)
    create(contents)
  }

  def widen(w: Int): Element = {
    if (w <= width) this
    else {
      val left = create(' ', (w - width) / 2, height)
      val right = create(' ', w - width - left.width, height)
      left beside this beside right
    }
  }

  def heighten(h: Int): Element = {
    if (h <= height) this
    else {
      val top = create(' ', width, (h - height) / 2)
      val bot = create(' ', width, h - height - top.height)
      top above this above bot
    }
  }

  def demo() {
    println("Element: demo")
  }
}

object Element {
  def create(contents: Array[String]): Element = {
    new ArrayElement(contents)
  }

  def create(line: String): Element = {
    new LineCompositionElement(line)
  }

  def create(character: Char, width: Int, height: Int): Element = {
    new UniformElement(character, width, height)
  }
}
