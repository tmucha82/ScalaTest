package org.coursera.week4

import java.util.NoSuchElementException

abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat

  def -(that: Nat): Nat

  def ==(that: Nat): Boolean = {
    try {
      (this - that).isZero
    } catch {
      case e: NoSuchElementException => false
    }
  }

}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if (that.isZero) this else throw new NoSuchElementException

  override def predecessor: Nothing = throw new NoSuchElementException
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor

  override def predecessor: Nat = n
}