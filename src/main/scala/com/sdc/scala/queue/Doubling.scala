package com.sdc.scala.queue

trait Doubling extends IntQueue {
  abstract override def put(element: Int): Unit = {
    super.put(element * 2)
  }
}
