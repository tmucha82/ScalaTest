package org.coursera.scala.parallel.week1

object UniqueIdApp1 extends App {

  private var uniqueId: Long = 0L

  def getUniqueId: Long = {
    uniqueId = uniqueId + 1
    uniqueId
  }

  def startThread(): Thread = {
    val thread = new Thread {
      override def run() = {
        val result = for (i <- 0 until 10) yield getUniqueId
        println(result)
      }
    }
    thread.start()
    thread
  }

  startThread()
  startThread()
}
