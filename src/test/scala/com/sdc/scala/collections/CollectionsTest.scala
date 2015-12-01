package com.sdc.scala.collections

import org.scalatest.FunSuite

import scala.collection.immutable.Nil
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class CollectionsTest extends FunSuite {

  test("List buffer") {
    var buffer = new ListBuffer[Int]
    assert(Nil === buffer)

    buffer += 1
    assert(ListBuffer(1) === buffer)
    buffer += 2
    assert(ListBuffer(1, 2) === buffer)

    buffer = 3 +: buffer
    assert(ListBuffer(3, 1, 2) === buffer)

    assert(List(3, 1, 2) === buffer.result())
    assert(3 === buffer.length)
    assert(ListBuffer(3) === buffer.take(1))
    assert(ListBuffer(3, 1) === buffer.take(2))
    assert(2 === buffer(2))
  }

  test("Array buffer") {
    var buffer = new ArrayBuffer[Int]

    buffer += 1
    assert(ArrayBuffer(1) === buffer)
    buffer += 2
    assert(ArrayBuffer(1, 2) === buffer)

    buffer = 3 +: buffer
    assert(ListBuffer(3, 1, 2) === buffer)

    assert(List(3, 1, 2) === buffer.result())
    assert(3 === buffer.length)
    assert(ListBuffer(3) === buffer.take(1))
    assert(ListBuffer(3, 1) === buffer.take(2))
    assert(2 === buffer(2))
  }

  test("Queue") {
    //TODO
  }

  test("Stack") {
    //TODO
  }

  test("String (via RichString)") {
    //TODO
  }

}
