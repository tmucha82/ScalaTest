package com.sdc.scala.queue

abstract class IntQueue {
  def get(): Int
  def put(element: Int)
}
