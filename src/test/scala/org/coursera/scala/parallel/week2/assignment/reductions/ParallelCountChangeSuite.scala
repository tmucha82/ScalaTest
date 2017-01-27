package org.coursera.scala.parallel.week2.assignment.reductions

import org.coursera.scala.parallel.week2.assignment.reductions.ParallelCountChange._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ParallelCountChangeSuite extends FunSuite {

  trait TestSet {
    def checkFunction(money: Int, coins: List[Int], countChangeFunction: (Int, List[Int]) => Int, expected: Int) =
      assert(countChangeFunction(money, coins) == expected, s"countChangeFunction($money, $coins) should be $expected")

    private val parallelCountChangeWithThreshold = (money: Int, coins: List[Int], threshold: Threshold) => parCountChange(_: Int, _: List[Int], threshold)

    val parallelCountChangeWithMoneyThreshold = (money: Int, coins: List[Int]) => parallelCountChangeWithThreshold(money, coins, moneyThreshold(money))
    val parallelCountChangeWithTotalCoinsThreshold = (money: Int, coins: List[Int]) => parallelCountChangeWithThreshold(money, coins, totalCoinsThreshold(coins.length))
    val parallelCountChangeWithCombinedThreshold = (money: Int, coins: List[Int]) => parallelCountChangeWithThreshold(money, coins, combinedThreshold(money, coins))

    def check(money: Int, coins: List[Int], expected: Int) = {
      checkFunction(money, coins, countChange, expected)
      checkFunction(money, coins, parallelCountChangeWithMoneyThreshold(money, coins), expected)
      checkFunction(money, coins, parallelCountChangeWithTotalCoinsThreshold(money, coins), expected)
      checkFunction(money, coins, parallelCountChangeWithCombinedThreshold(money, coins), expected)
    }
  }

  test("countChange and parCountChange should return 0 for money < 0") {
    new TestSet {
      val expected = 0

      check(-1, List(), expected)
      check(-1, List(1, 2, 3), expected)
      check(-Int.MinValue, List(), expected)
      check(-Int.MinValue, List(1, 2, 3), expected)
    }
  }

  test("countChange and parCountChange should return 1 when money == 0") {
    new TestSet {
      val money = 0
      val expected = 1

      check(money, List(), expected)
      check(money, List(1, 2, 3), expected)
      check(money, List.range(1, 100), expected)
    }
  }

  test("countChange should and parCountChange return 0 for money > 0 and coins = List()") {
    new TestSet {
      val coins = List()
      val expected = 0

      check(1, coins, expected)
      check(Int.MaxValue, coins, expected)
    }
  }

  test("countChange and parCountChange should work when there is only one coin") {
    new TestSet {
      check(1, List(1), 1)
      check(2, List(1), 1)
      check(1, List(2), 0)
      check(Int.MaxValue, List(Int.MaxValue), 1)
      check(Int.MaxValue - 1, List(Int.MaxValue), 0)
    }
  }

  test("countChange and parCountChange should work for multi-coins") {
    new TestSet {
      check(4, List(1, 2), 3)
      check(50, List(1, 2, 5, 10), 341)
      check(300, List(5, 10, 20, 50, 100, 200, 500), 1022)
      check(301, List(5, 10, 20, 50, 100, 200, 500), 0)
      check(250, List(1, 2, 5, 10, 20, 50), 177863)
    }
  }
}
