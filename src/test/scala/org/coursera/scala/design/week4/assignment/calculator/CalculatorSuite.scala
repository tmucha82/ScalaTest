package org.coursera.scala.design.week4.assignment.calculator

import org.coursera.scala.design.week4.assignment.calculator.TweetLength.MaxTweetLength
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, _}

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /** ****************
    * * TWEET LENGTH **
    * *****************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a changing signal") {
    val input = Var("hello world")
    val result = TweetLength.tweetRemainingCharsCount(input)
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val inputValue = input()
    input() = inputValue + " !!!"
    assert(result() == MaxTweetLength - tweetLength("hello world !!!"))

    val tooLong = "foo" * 200
    val tooLongInput = Var(tooLong)
    val result2 = TweetLength.tweetRemainingCharsCount(tooLongInput)
    assert(result2() == MaxTweetLength - tweetLength(tooLong))

    val tooLongInputValue = tooLongInput()
    tooLongInput() = tooLongInputValue.take(100)
    assert(result2() == MaxTweetLength - tweetLength(tooLong.take(100)))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }

  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("colorForRemainingCharsCount with a changing signal") {
    val inputCount = Var(MaxTweetLength) //140
    var currentCount = inputCount()
    val color = TweetLength.colorForRemainingCharsCount(inputCount)

    assert(color() == "green")

    currentCount = inputCount()
    inputCount() = currentCount - 100 //40
    assert(color() == "green")

    currentCount = inputCount()
    inputCount() = currentCount - 30 //10
    assert(color() == "orange")

    currentCount = inputCount()
    inputCount() = currentCount - 10 //0
    assert(color() == "orange")

    currentCount = inputCount()
    inputCount() = currentCount - 20 //-20
    assert(color() == "red")
  }

  /** ****************
    * * POLYNOMIAL  **
    * ****************/
  test("computeDelta with constant signal") {
    val twoSolutions = Polynomial.computeDelta(Var(-2), Var(3), Var(-1))
    assert(twoSolutions() === 1)

    val noSolution = Polynomial.computeDelta(Var(1), Var(2), Var(4))
    assert(noSolution() === -12)

    val oneSolution = Polynomial.computeDelta(Var(4), Var(4), Var(1))
    assert(oneSolution() === 0)
  }

  test("computeDelta with changing signal") {
    val a = Var(-2.0)
    val b = Var(3.0)
    val c = Var(-1.0)
    val delta = Polynomial.computeDelta(a, b, c)
    assert(delta() === 1)

    a() = 1
    b() = 2
    c() = 4
    assert(delta() === -12)

    a() = 4
    b() = 4
    c() = 1
    assert(delta() === 0)
  }

  test("computeSolutions with changing signal") {
    val a = Var(-2.0)
    val b = Var(3.0)
    val c = Var(-1.0)
    val delta = Polynomial.computeDelta(a, b, c)
    val solutions = Polynomial.computeSolutions(a, b, c, delta)
    assert(solutions() === Set(0.5, 1.0))

    a() = 1
    b() = 2
    c() = 4
    assert(solutions() === Set.empty)

    a() = 4
    b() = 4
    c() = 1
    assert(solutions() === Set(-0.5))
  }
}
