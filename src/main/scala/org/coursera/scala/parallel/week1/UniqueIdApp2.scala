package org.coursera.scala.parallel.week1

object UniqueIdApp2 extends App {

  private val x = new AnyRef {}
  private var uniqueId: Long = 0L

  def getUniqueId: Long = x.synchronized {
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
