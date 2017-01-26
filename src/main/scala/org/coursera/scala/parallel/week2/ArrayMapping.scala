package org.coursera.scala.parallel.week2

import org.coursera.scala.parallel.common._
import org.scalameter.{Warmer, _}

object ArrayMapping {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 60,
    Key.exec.benchRuns -> 60,
    Key.verbose -> true
  ) withWarmer new Warmer.Default


  def initialize(length: Int): Array[Int] = {
    val array = new Array[Int](length)
    for(i <- 0 until length) array(i) = i % 100
    array
  }

  def mapSegmentSeq[A, B](in: Array[A], out: Array[B], start: Int, end: Int, f: A => B): Array[B] = {
    for (i <- start until end) out(i) = f(in(i))
    out
  }

  def mapSegmentPar[A, B](in: Array[A], out: Array[B], start: Int, end: Int, f: A => B)(threshold: Int = 2): Array[B] = {
    // Writes to out(i) for left <= i <= right-1
    if (end - start < threshold)
      mapSegmentSeq(in, out, start, end, f)
    else {
      val mid = start + (end - start) / 2
      parallel(
        mapSegmentPar(in, out, start, mid, f)(threshold),
        mapSegmentPar(in, out, mid, end, f)(threshold)
      )
      out
    }
  }

  def mapSeq[A, B](in: Array[A], out: Array[B], f: A => B) = mapSegmentSeq(in, out, 0, in.length, f)

  def mapPar[A, B](in: Array[A], out: Array[B], f: A => B) = mapSegmentPar(in, out, 0, in.length, f)(2)

  def main(args: Array[String]) {
    val length = 10000000
    val input = initialize(length)
    val f = (x: Int) => x * x * x

    val seqtime = standardConfig measure {
      mapSeq(input, new Array[Int](length), f)
    }
    println(s"sequential sum time: ${seqtime.value}")

    val partime = standardConfig measure {
      mapPar(input, new Array[Int](length), f)
    }
    println(s"fork / join time: ${partime.value}")
    println(s"speedup: ${seqtime.value / partime.value}")
  }

}