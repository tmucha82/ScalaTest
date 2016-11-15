package com.sdc.scala.queue.basic

abstract class IntQueue {
  def get(): Int

  def put(element: Int)
}
