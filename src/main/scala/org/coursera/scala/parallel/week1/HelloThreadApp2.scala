package org.coursera.scala.parallel.week1

object HelloThreadApp2 extends App {

  class HelloThread extends Thread {
    override def run(): Unit = {
      println("Hello")
      println("World!")
    }
  }

  val thread1 = new HelloThread
  val thread2 = new HelloThread
  thread1.start()
  thread2.start()
  thread1.join()
  thread2.join()
}
