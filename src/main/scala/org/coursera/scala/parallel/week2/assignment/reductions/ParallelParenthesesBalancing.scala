package org.coursera.scala.parallel.week2.assignment.reductions

import org.coursera.scala.parallel.common._
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

  /**
    * Returns `true` iff the parentheses in the input `chars` are balanced.
    *
    * @param chars     input string as chars array
    * @param threshold parameter for parallel algorithm
    * @return `true` if the balancing of parentheses is met
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(from: Int, until: Int, openingParenthesesBalance: Int, closingParenthesesBalance: Int): (Int, Int) = {
      /**
        * Computes balance of opening and closing parentheses of given string.
        * Function analogous balance for seq
        *
        * @param chars                     input string as chars array
        * @param openingParenthesesBalance balance of opening parentheses: +1 if we find `(`, -1 if we find `)` but >0
        * @param closingParenthesesBalance balance of closing parentheses: +1
        * @return accumulator of openingParenthesesBalance and closingParenthesesBalance
        */
      def balance(chars: Array[Char], openingParenthesesBalance: Int, closingParenthesesBalance: Int): (Int, Int) = {
        if (chars.isEmpty) (openingParenthesesBalance, closingParenthesesBalance)
        else if (chars.head == '(') balance(chars.tail, openingParenthesesBalance + 1, closingParenthesesBalance)
        else if (chars.head == ')') {
          if (openingParenthesesBalance > 0) balance(chars.tail, openingParenthesesBalance - 1, closingParenthesesBalance)
          else balance(chars.tail, openingParenthesesBalance, closingParenthesesBalance + 1)
        }
        else balance(chars.tail, openingParenthesesBalance, closingParenthesesBalance)
      }

      balance(chars.slice(from, until), openingParenthesesBalance, closingParenthesesBalance)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val middle = (from + until) / 2
        val ((leftOpeningBalance, leftClosingBalance), (rightOpeningBalance, rightClosingBalance)) = parallel(reduce(from, middle), reduce(middle, until))
        val matched = math.min(leftOpeningBalance, rightClosingBalance)
        (leftOpeningBalance + rightOpeningBalance - matched, leftClosingBalance + rightClosingBalance - matched)
      }
    }

    reduce(0, chars.length) ==(0, 0)
  }
}
