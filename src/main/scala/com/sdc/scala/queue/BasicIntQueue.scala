package com.sdc.scala.queue

//you can do also that - sic! :P
import scala.collection.mutable.{ArrayBuffer => Buffer}

class BasicIntQueue extends IntQueue {
  private val buffer = new Buffer[Int]

  override def get(): Int = {
    buffer.remove(0)
  }

  override def put(element: Int): Unit = buffer += element
}
