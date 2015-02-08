package com.sdc.scala.car

import com.google.common.base.Objects

class Car(_color: String, _valid: Boolean) {
  val color: String = _color
  val valid: Boolean = _valid

  override def toString = Objects.toStringHelper(this).add("color", color).add("valid", valid).toString
}
