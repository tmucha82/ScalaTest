package org.coursera.scala.parallel.week2

import org.coursera.scala.parallel.common._
import org.scalameter.{Warmer, _}


object Mapping {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 60,
    Key.exec.benchRuns -> 60,
    Key.verbose -> true
  ) withWarmer new Warmer.Default


  def initialize(length: Int): List[Int] = {
    val list = Nil
    for (i <- 0 until length) {
      (i % 100) :: list
    }
    list
  }

  def mapSeq[A, B](list: List[A], f: A => B): List[B] = list match {
    case Nil => Nil
    case head :: tail => f(head) :: mapSeq(tail, f)
  }

  //different approach (finding middle element) is not good choice - list is not ideal ofr such parallelism
  def mapPar[A, B](list: List[A], f: A => B): List[B] = list match {
    case Nil => Nil
    case head :: tail =>
      val (a, b) = parallel(f(head), mapPar(tail, f))
      a :: b
  }

  def main(args: Array[String]) {
    val length = 10000000
    val inputList = initialize(length)
    val f = (x: Int) => x * x * x

    val seqtime = standardConfig measure {
      mapSeq(inputList, f)
    }
    println(s"sequential sum time: $seqtime")

    val partime = standardConfig measure {
      mapPar(inputList, f)
    }
    println(s"fork / join time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }

}