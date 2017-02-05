package org.coursera.scala.parallel.week4

import org.scalatest.FunSuite

class ConcTest extends FunSuite {

  test("testLevel") {
    val tree = <>(new Singleton(3), new Singleton(4))
    assert(tree.size === 2)
    assert(tree.height === 1)
  }
}
