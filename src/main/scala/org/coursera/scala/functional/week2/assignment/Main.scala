package org.coursera.scala.functional.week2.assignment

object Main extends App {

  import FunSets._

  println(contains(singletonSet(1), 1))
  printSet(filter(x => x > 0, x => x % 2 == 0))
}
