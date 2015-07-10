package com.sdc.scala.element

abstract class Element {
  def contents: Array[String]

  def height: Int = contents.length

  def width: Int = if (height != 0) contents(0).length else 0
}
