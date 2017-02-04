package org.coursera.scala.parallel.week4

import org.scalatest.FunSuite

import scala.collection.mutable.ArrayBuffer


class ArrayCombinerTest extends FunSuite {

  trait TestSet {
    val arrayCombiner: ArrayCombiner[String] = new ArrayCombiner[String](2)
    val anotherCombiner: ArrayCombiner[String] = new ArrayCombiner[String](2)
    val combiner1 = new ArrayCombiner[String](2)
    val combiner2 = new ArrayCombiner[String](2)
  }

  test("+= method") {
    new TestSet {
      arrayCombiner += "1"
      arrayCombiner += "2"
      arrayCombiner += "3"
      arrayCombiner += "4"
      assert(arrayCombiner.currentNumElems === 4)
      assert(arrayCombiner.currentBuffers === ArrayBuffer(ArrayBuffer("1", "2", "3", "4")))
    }
  }

  test("combine method") {
    new TestSet {
      combiner1 += "1"
      combiner1 += "2"
      combiner2 += "3"
      combiner2 += "4"
      val result = combiner1.combine(combiner2)
      assert(result.asInstanceOf[ArrayCombiner[String]].currentNumElems === 4)
      assert(result.asInstanceOf[ArrayCombiner[String]].currentBuffers === ArrayBuffer(ArrayBuffer("1", "2"), ArrayBuffer("3", "4")))
    }
  }

  test("result method") {
    new TestSet {
      arrayCombiner += "1"
      arrayCombiner += "2"
      arrayCombiner += "3"
      arrayCombiner += "4"
      anotherCombiner += "5"
      anotherCombiner += "6"
      val finalCombiner = arrayCombiner.combine(anotherCombiner)
      assert(finalCombiner.result === Array("1", "2", "3", "4", "5", "6"))
    }
  }
}
