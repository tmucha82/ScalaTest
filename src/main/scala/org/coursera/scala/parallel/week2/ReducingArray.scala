package org.coursera.scala.parallel.week2

import org.coursera.scala.parallel.common._
import org.scalameter.{Warmer, _}

object ReducingArray {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 60,
    Key.exec.benchRuns -> 60,
    Key.verbose -> true
  ) withWarmer new Warmer.Default


  def initialize(length: Int): Array[Int] = {
    val array = new Array[Int](length)
    for (i <- 0 until length) array(i) = i % 100
    array
  }

  def reduceSegmentSeq[A](in: Array[A], start: Int, end: Int, f: (A, A) => A): A = {
    var res = in(start)
    for (i <- start + 1 until end) {
      res = f(res, in(i))
    }
    res
  }

  def reduceSegmentPar[A](in: Array[A], start: Int, end: Int, f: (A, A) => A)(threshold: Int = 2): A = {
    if (end - start < threshold) {
      reduceSegmentSeq(in, start, end, f)
    } else {
      val mid = start + (end - start) / 2
      val (a1, a2) = parallel(
        reduceSegmentPar(in, start, mid, f)(threshold),
        reduceSegmentPar(in, mid, end, f)(threshold)
      )
      f(a1, a2)
    }
  }

  def reduceSeq[A](in: Array[A], f: (A, A) => A): A = reduceSegmentSeq(in, 0, in.length, f)

  def reducePar[A](in: Array[A], f: (A, A) => A): A = reduceSegmentPar(in, 0, in.length, f)(1000)


  def main(args: Array[String]) {
    val length = 10000000
    val input = initialize(length)
    val f = (x: Int, y: Int) => x + y

    val seqtime = standardConfig measure {
      reduceSeq(input, f)
    }
    println(s"sequential sum time: $seqtime")

    val partime = standardConfig measure {
      reducePar(input, f)
    }
    println(s"fork / join time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }

}