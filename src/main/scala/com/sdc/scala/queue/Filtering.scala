package com.sdc.scala.queue

trait Filtering extends IntQueue {
  abstract override def put(element: Int): Unit = if (element > 0) super.put(element)
}
