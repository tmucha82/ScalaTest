package com.sdc.scala.abstraction

class Concrete extends Abstract {
  override type T = this.type

  override def transform(x: T): T = this

  override val initial: T = this
  override var current: T = this
}
