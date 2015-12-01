package com.sdc.scala.collections

import org.scalatest.FunSuite

import scala.collection.immutable.Nil
import scala.collection.mutable.ListBuffer

class CollectionsTest extends FunSuite {

  test("List buffer") {
    val buffer = new ListBuffer[Int]
    assert(Nil === buffer)
  }
}
