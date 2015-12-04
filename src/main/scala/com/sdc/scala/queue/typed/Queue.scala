package com.sdc.scala.queue.typed

class Queue[+T] private(private[this] var leading: List[T], private[this] var trailing: List[T]) {

  private def mirror() =
    if (leading.isEmpty) {
      while (trailing.nonEmpty) {
        leading = trailing.head :: leading
        trailing = trailing.tail
      }
    }

  def head: T = {
    mirror()
    leading.head
  }

  def tail: Queue[T] = {
    mirror()
    new Queue(leading.tail, trailing)
  }

  def append[U >: T](x: U) =
    new Queue[U](leading, x :: trailing)
}

object Queue {
  def apply[T](elements: T*): Queue[T] = new Queue(elements.toList, Nil)
}