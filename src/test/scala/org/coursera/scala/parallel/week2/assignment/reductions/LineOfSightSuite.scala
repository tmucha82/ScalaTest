package org.coursera.scala.parallel.week2.assignment.reductions

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LineOfSightSuite extends FunSuite {

  import LineOfSight._

  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val input = Array[Float](0f, 1f, 8f, 9f)
    assert(upsweepSequential(input, 1, 4) === 4f)
    assert(upsweepSequential(input, 0, 4) === 4f)
  }

  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("lineOfSight and parLineOfSight should gave the same result") {
    val input = Array[Float](0f, 1f, 8f, 9f)
    val output1 = new Array[Float](4)
    val output2 = new Array[Float](4)
    lineOfSight(input, output1)
    parLineOfSight(input, output2, 2)
    assert(output1 === output2)
  }

  test("lineOfSight and parLineOfSight should gave the same result - bigger input") {
    val length = 1000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output1 = new Array[Float](length + 1)
    val output2 = new Array[Float](length + 1)

    lineOfSight(input, output1)
    parLineOfSight(input, output2, 10)
    assert(output1 === output2)
  }
}

