package com.sdc.scala.rational

import scala.language.implicitConversions

class Rational(n: Int, d: Int) {
  require(d != 0)
  val number: Int = n
  val denominator: Int = d

  def this(n: Int) = this(n, 1)

  override def toString: String = n + "/" + d

  def add(that: Rational): Rational = new Rational(number * that.denominator + that.number * denominator, denominator * that.denominator)

  def lessThan(that: Rational): Boolean = this.number * that.denominator < that.number * this.denominator

  def max(that: Rational) = if (this.lessThan(that)) that else this

  private def greatestCommonDivisor(a: Int, b: Int): Int = if (b == 0) a else greatestCommonDivisor(b, a % b)

  def +(that: Rational): Rational = new Rational(number * that.denominator + that.number * denominator, denominator * that.denominator)

  def + (i: Int): Rational = this + new Rational(i)

  def - (that: Rational): Rational = new Rational(number * that.denominator - that.number * denominator, denominator * that.denominator)

  def - (i: Int): Rational = this - new Rational(i)

  def *(that: Rational): Rational = new Rational(number * that.number, denominator * that.denominator)

  def * (i: Int): Rational = this * new Rational(i)

  def / (that: Rational): Rational = new Rational(number * that.denominator, denominator * that.number)

  def / (i: Int): Rational = this / new Rational(i)
}

object Rational {

  implicit def intToRational(x: Int): Rational = new Rational(x)

}
