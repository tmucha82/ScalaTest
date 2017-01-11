package org.coursera.scala.parallel.week1

object AccountThreadApp1 extends App {

  class Account(private var amount: Int = 0) {
    def currentAmount = amount

    def transfer(target: Account, n: Int) = this.synchronized {
      target synchronized {
        this.amount -= n
        target.amount += n
        println(s"Successfully transferred $n from $this to $target")
      }
    }
  }

  def startThread(a: Account, b: Account, n: Int) = {
    val thread = new Thread {
      override def run() = {
        for (i <- 0 until n) {
          a.transfer(b, 1)
        }
      }
    }
    thread.start()
    thread
  }

  val a = new Account(500000)
  val b = new Account(700000)

  val thread1 = startThread(a, b, 15000)
  val thread2 = startThread(b, a, 15000)
  /// possible deadlock
  thread1.join()
  thread2.join()

  println(s"amount of a = ${a.currentAmount}")
  println(s"amount of b = ${b.currentAmount}")

}
