package com.sdc.scala.abstraction


trait Abstract {
  type T
  def transform(x: T): T
  val initial: T
  var current: T
}
