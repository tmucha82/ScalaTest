package org.coursera.scala.parallel.week2.assignment.reductions

import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer new Warmer.Default

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing {

  /**
    * Returns `true` iff the parentheses in the input `chars` are balanced.
    *
    * @param chars string which is verified against the balancing of parentheses
    * @return `true` if the balancing of parentheses is met
    */
  def balance(chars: Array[Char]): Boolean = {
    def parenthesesStockSize(chars: Array[Char], initBalanceStockSize: Int): Int = {
      if (initBalanceStockSize < 0) -1
      else {
        if (chars.isEmpty) initBalanceStockSize
        else if (chars.head == '(') parenthesesStockSize(chars.tail, initBalanceStockSize + 1)
        else if (chars.head == ')') parenthesesStockSize(chars.tail, initBalanceStockSize - 1)
        else parenthesesStockSize(chars.tail, initBalanceStockSize)
      }
    }
    parenthesesStockSize(chars, 0) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) /*: ???*/ = {
      ???
    }

    def reduce(from: Int, until: Int) /*: ???*/ = {
      ???
    }

    reduce(0, chars.length) == ???
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
