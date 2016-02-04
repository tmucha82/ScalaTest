package com.sdc.scala.forLoop

import org.scalatest.FunSuite

class ForLoopTest extends FunSuite {

  case class Person(name: String, isMale: Boolean, children: Person*)

  val lara = Person("Lara", isMale = false)
  val bob = Person("Bob", isMale = true)
  val julie = Person("Julie", false, lara, bob)
  val tom = Person("Tom", true, bob)
  val persons = List(lara, bob, julie, tom)

  test("simple test") {
    val women = persons filter (p => !p.isMale)
    assert(List(lara, julie) === women)
    //you could find mother with child like that....
    var motherWithChild = persons filter (p => !p.isMale) flatMap (p => p.children map (c => (p.name, c.name)))
    assert(List(("Julie", "Lara"), ("Julie", "Bob")) === motherWithChild)

    //...or. Interestingly compiler will translate the second query into the first one
    motherWithChild = for (person <- persons; if !person.isMale; child <- person.children) yield (person.name, child.name)
    assert(List(("Julie", "Lara"), ("Julie", "Bob")) === motherWithChild)
  }

  test("for expression both ways") {
    //generators, definitions and filters
    var toNames = for (p <- persons; n = p.name; if n startsWith "To") yield n
    assert(List("Tom") === toNames)

    toNames = for {
      p <- persons //generator
      n = p.name //definition
      if n startsWith "To" //filter
    } yield n
    assert(List("Tom") === toNames)


    val testList = for (x <- List(1, 2); y <- List("one", "two")) yield (x, y)
    //assert(List((1, "one"), (1, "two"), (2, "one"), (2, "two")))
  }
}
