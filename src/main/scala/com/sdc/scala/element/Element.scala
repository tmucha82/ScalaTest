package com.sdc.scala.element

import com.sdc.scala.element.Element.create

abstract class Element {
  def contents: Array[String]

  def height: Int = contents.length

  def width: Int = if (height != 0) contents(0).length else 0

  def above(that: Element): Element = {
    val widenThis = this widen that.width
    val widenThat = that widen this.width
    create(widenThis.contents ++ widenThat.contents)
  }

  def beside(that: Element): Element = {
    val heightenThis = this heighten that.height
    val heightenThat = that heighten this.height
    create(
//      for (element <- this.contents.zip(that.contents)) yield element._1 + element._2// you could also
      for ((line1, line2) <- heightenThis.contents.zip(heightenThat.contents)) yield line1 + line2
    )
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

  override def toString: String = contents.mkString("\n")
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
