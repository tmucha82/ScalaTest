package com.sdc.scala.numbers

object Numbers {
  def half(input: Int) = if (input % 2 == 0) input / 2 else throw new IllegalArgumentException("n must be even")
}
