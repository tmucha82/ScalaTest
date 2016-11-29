package org.coursera.week4

/**
  * Related to List from week3
  */

import java.util.NoSuchElementException

import org.coursera.week3.assignment.objsets.{Empty, NonEmpty}

trait List[+T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]

  //  def prepend(elem: T): List[T] = new Cons(elem, this) //we cannot do that - but we can do (upper) bound
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}

object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true

  override def tail: Nothing = throw new NoSuchElementException

  override def head: Nothing = throw new NoSuchElementException
}

object List {
  def apply[T](x: T, y: T): List[T] = new Cons[T](x, apply(y))

  def apply[T](x: T): List[T] = new Cons[T](x, apply())

  def apply[T](): List[T] = Nil[T]
}

object test {
  val x: List[String] = Nil

  def f(xs: List[NonEmpty], x: Empty) = xs.prepend(x) //returns List[IntSet]
}
