package org.coursera.scala.parallel.week4

import org.scalatest._

class ConcSpec extends FlatSpec {
  "A Conc" should "have size 1000 after appending 1000 elements" in {
    var c: Conc[Int] = Empty
    for (i <- 1 to 1000) {
      c = c.appendLeaf(Singleton(10))
    }
    assert(c.size == 1000)
  }
}