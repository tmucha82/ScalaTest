package com.sdc.scala.rational

import scala.language.implicitConversions

class Rational(n: Int, d: Int) extends Ordered[Rational] {
  require(d != 0)
  val number: Int = n
  val denominator: Int = d

  def this(n: Int) = this(n, 1)

  override def toString: String = if (denominator == 1) number.toString else number + "/" + denominator

  def add(that: Rational): Rational = new Rational(number * that.denominator + that.number * denominator, denominator * that.denominator)

  def lessThan(that: Rational): Boolean = this.number * that.denominator < that.number * this.denominator

  def max(that: Rational) = if (this.lessThan(that)) that else this

  def +(that: Rational): Rational = new Rational(number * that.denominator + that.number * denominator, denominator * that.denominator)

  def +(i: Int): Rational = this + new Rational(i)

  def -(that: Rational): Rational = new Rational(number * that.denominator - that.number * denominator, denominator * that.denominator)

  def -(i: Int): Rational = this - new Rational(i)

  def *(that: Rational): Rational = new Rational(number * that.number, denominator * that.denominator)

  def *(i: Int): Rational = this * new Rational(i)

  def /(that: Rational): Rational = new Rational(number * that.denominator, denominator * that.number)

  def /(i: Int): Rational = this / new Rational(i)

  override def compare(that: Rational): Int = (this.number * that.denominator) - (that.number * this.denominator)

  //could be val or def - depending on performance - caching hash code (only for immutable!)
  override def hashCode: Int = 41 * (41 + number) + denominator

  override def equals(other: Any): Boolean = other match {
    case that: Rational =>
      (that canEqual this) &&
        number == that.number && denominator == that.denominator
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Rational]
}

object Rational {

  implicit def intToRational(x: Int): Rational = new Rational(x)

  def greatestCommonDivisor(a: Int, b: Int): Int = if (b == 0) a else greatestCommonDivisor(b, a % b)

}
