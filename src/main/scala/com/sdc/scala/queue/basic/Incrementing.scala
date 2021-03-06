package com.sdc.scala.queue.basic

trait Incrementing extends IntQueue {
  abstract override def put(element: Int): Unit = super.put(element + 1)
}
