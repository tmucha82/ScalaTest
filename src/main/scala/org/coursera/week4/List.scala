package org.coursera.week4

/**
  * Related to List from week3
  */

import java.util.NoSuchElementException

trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true

  override def tail: Nothing = throw new NoSuchElementException

  override def head: Nothing = throw new NoSuchElementException
}

object List {
  def apply[T](x: T, y: T): List[T] = new Cons[T](x, apply(y))

  def apply[T](x: T): List[T] = new Cons[T](x, apply())

  def apply[T](): List[T] = new Nil[T]
}
