package org.coursera.scala.parallel.week1

object HelloThreadApp1 extends App {

  class HelloThread extends Thread {
    override def run(): Unit = {
      println("Hello World!")
    }
  }

  val thread = new HelloThread
  thread.start()
  thread.join()
}
