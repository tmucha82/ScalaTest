package com.sdc.scala.element

class UniformElement(character: Char, override val height: Int, override val width: Int) extends Element {
  private val line = character.toString * width

  override def contents: Array[String] = Array.fill(height)(line)

  override def demo() {
    println("UniformElement: demo")
  }
}
