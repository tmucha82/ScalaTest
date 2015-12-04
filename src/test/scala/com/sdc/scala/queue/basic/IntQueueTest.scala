package com.sdc.scala.queue.basic

import org.scalatest.FunSuite

class IntQueueTest extends FunSuite {
  test("put and get from queue") {

    val queue: IntQueue = new BasicIntQueue
    queue.put(1)
    queue.put(2)
    queue.put(3)

    assert(1 === queue.get())
    assert(2 === queue.get())

    queue.put(4)
    assert(3 === queue.get())
    assert(4 === queue.get())
    intercept[IndexOutOfBoundsException] {
      queue.get()
    }
  }

  test("doubling int queue") {
    val queue: IntQueue = new DoublingBasicQueue
    queue.put(1)
    queue.put(2)
    queue.put(3)

    assert(2 === queue.get())
    assert(4 === queue.get())

    queue.put(4)
    assert(6 === queue.get())
    assert(8 === queue.get())
    intercept[IndexOutOfBoundsException] {
      queue.get()
    }
  }

  test("doubling int queue anonymously") {
    val queue: IntQueue = new BasicIntQueue with Doubling
    queue.put(1)
    queue.put(2)
    queue.put(3)

    assert(2 === queue.get())
    assert(4 === queue.get())

    queue.put(4)
    assert(6 === queue.get())
    assert(8 === queue.get())
    intercept[IndexOutOfBoundsException] {
      queue.get()
    }
  }

  test("incrementing int queue") {
    val queue: IntQueue = new BasicIntQueue with Incrementing
    queue.put(1)
    queue.put(2)
    queue.put(3)

    assert(2 === queue.get())
    assert(3 === queue.get())

    queue.put(4)
    assert(4 === queue.get())
    assert(5 === queue.get())
    intercept[IndexOutOfBoundsException] {
      queue.get()
    }
  }

  test("filtering int queue") {
    val queue: IntQueue = new BasicIntQueue with Filtering
    queue.put(1)
    queue.put(-2)
    queue.put(3)

    assert(1 === queue.get())
    assert(3 === queue.get())

    queue.put(-4)
    intercept[IndexOutOfBoundsException] {
      queue.get()
    }
  }

  test("filtering and incrementing int queue") {
    //!! first would be filtering then incrementing. Generally direction of order is <------
    val queue: IntQueue = new BasicIntQueue with Incrementing with Filtering
    queue.put(1)
    queue.put(-2)
    queue.put(3)

    assert(2 === queue.get())
    assert(4 === queue.get())

    queue.put(-4)
    intercept[IndexOutOfBoundsException] {
      queue.get()
    }
  }

}
