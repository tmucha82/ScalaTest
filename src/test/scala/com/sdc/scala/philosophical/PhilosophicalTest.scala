package com.sdc.scala.philosophical

import org.scalatest.FunSuite

class PhilosophicalTest extends FunSuite {
  test("element factory") {
    val frog = new Frog
    assert(frog.toString === "green")

    val philosophical: Philosophical = frog
    assert(philosophical.toString === "green")

    frog.philosophize() //It ain't easy being green!
    philosophical.philosophize() //It ain't easy being green!
  }

}
