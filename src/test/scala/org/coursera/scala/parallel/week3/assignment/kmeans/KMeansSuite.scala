package org.coursera.scala.parallel.week3.assignment.kmeans

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.collection._

object KM extends KMeans

import org.coursera.scala.parallel.week3.assignment.kmeans.KM._

@RunWith(classOf[JUnitRunner])
class KMeansSuite extends FunSuite {

  trait TestSet {
    val points: GenSeq[Point] = IndexedSeq(
      new Point(1, 1, 0),
      new Point(1, -1, 0),
      new Point(-1, 1, 0),
      new Point(-1, -1, 0)
    )

    val oneMeanZero: GenSeq[Point] = IndexedSeq(new Point(0, 0, 0))
    val oneMeanOne: GenSeq[Point] = IndexedSeq(new Point(1, 1, 1))
    val twoMeans: GenSeq[Point] = IndexedSeq(new Point(1, 0, 0), new Point(-1, 0, 0))
  }

  def checkClassify(points: GenSeq[Point], means: GenSeq[Point], expected: GenMap[Point, GenSeq[Point]]) {
    assert(classify(points, means) == expected,
      s"classify($points, $means) should equal to $expected")
  }

  test("'classify should work for empty 'points' and empty 'means'") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq()
    val expected = GenMap[Point, GenSeq[Point]]()
    checkClassify(points, means, expected)
  }

  test("'classify' should work for empty 'points' and 'means' == GenSeq(Point(1,1,1))") {
    new TestSet {
      val expected = GenMap[Point, GenSeq[Point]]((oneMeanOne.head, GenSeq()))
      checkClassify(IndexedSeq(), oneMeanOne, expected)
    }
  }

  test("'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((0, 0, 0))") {
    new TestSet {
      val expected = GenMap((oneMeanZero.head, points))
      checkClassify(points, oneMeanZero, expected)
    }
  }

  test("'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((1, 0, 0), (-1, 0, 0))") {
    new TestSet {
      val expected = GenMap((twoMeans.head, GenSeq(points.head, points(1))), (twoMeans(1), GenSeq(points(2), points(3))))
      checkClassify(points, twoMeans, expected)
    }
  }

  def checkParClassify(points: GenSeq[Point], means: GenSeq[Point], expected: GenMap[Point, GenSeq[Point]]) {
    assert(classify(points.par, means.par) == expected, s"classify($points par, $means par) should equal to $expected")
  }

  test("'classify with data parallelism should work for empty 'points' and empty 'means'") {
    checkParClassify(IndexedSeq(), IndexedSeq(), GenMap[Point, GenSeq[Point]]())
  }

  test("update") {
    val initMeans: GenSeq[Point] = IndexedSeq(new Point(1, 0, 0), new Point(-1, 0, 0))
    //    update(classified, initMeans)
  }
}


  
