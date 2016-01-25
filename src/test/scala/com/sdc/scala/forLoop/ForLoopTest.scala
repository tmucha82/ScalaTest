package com.sdc.scala.forLoop

import org.scalatest.FunSuite

class ForLoopTest extends FunSuite {

  case class Person(name: String, isMale: Boolean, children: Person*)

  test("simple test") {

    val lara = Person("Lara", isMale = false)
    val bob = Person("Bob", isMale = true)
    val julie = Person("Julie", false, lara, bob)
    val persons = List(lara, bob, julie)


    val women = persons filter (p => !p.isMale)
    assert(List(lara, julie) === women)
    val motherWithChild = women flatMap (p => p.children map (c => (p.name, c.name)))
    assert(List(("Julie", "Lara"), ("Julie", "Bob")) === motherWithChild)
  }

}
