package scala.rational

import com.sdc.scala.rational.Rational
import org.scalatest.FunSuite

class RationalTest extends FunSuite {

  test("simple construction of rational") {
    val rational = new Rational(10, 2)
    assert(rational.number === 10)
    assert(rational.denominator === 2)
  }

  test("add one rational with other") {
    val rational = new Rational(6, 3).add(new Rational(6, 2))
    assert(rational.number === 30)
    assert(rational.denominator === 6)
  }

  test("test if one rational is less than another") {
    assert(new Rational(6, 3).lessThan(new Rational(6, 2)))
    assert(!new Rational(6, 2).lessThan(new Rational(6, 3)))
    assert(!new Rational(6, 3).lessThan(new Rational(6, 3)))
  }

  test("max of two rationals") {
    val result = new Rational(6, 3).max(new Rational(6, 2))
    assert(result.number === 6)
    assert(result.denominator === 2)
  }

  test("auxiliary constructor") {
    val result = new Rational(5)
    assert(result.number === 5)
    assert(result.denominator === 1)
  }

  test("operator + with rational") {
    val rational = new Rational(6, 3) + new Rational(6, 2)
    assert(rational.number === 30)
    assert(rational.denominator === 6)
  }

  test("operator + with int") {
    val rational = new Rational(6, 3) + 3
    assert(rational.number === 15)
    assert(rational.denominator === 3)
  }

  test("operator - with rational") {
    val rational = new Rational(6, 2) - new Rational(6, 3)
    assert(rational.number === 6)
    assert(rational.denominator === 6)
  }

  test("operator - with int") {
    val rational = new Rational(6, 2) - 2
    assert(rational.number === 2)
    assert(rational.denominator === 2)
  }

  test("operator * with rational") {
    val rational = new Rational(6, 3) * new Rational(6, 2)
    assert(rational.number === 36)
    assert(rational.denominator === 6)
  }

  test("operator * with int") {
    val rational = new Rational(6, 3) * 3
    assert(rational.number === 18)
    assert(rational.denominator === 3)
  }

  test("operator / with rational") {
    val rational = new Rational(6, 3) / new Rational(6, 2)
    assert(rational.number === 12)
    assert(rational.denominator === 18)
  }

  test("operator / with int") {
    val rational = new Rational(6, 2) / 2
    assert(rational.number === 6)
    assert(rational.denominator === 4)
  }

}
