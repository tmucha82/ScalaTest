package org.coursera.scala.design.week4

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure, Random}

object FutureTest extends App {


  def sleep(time: Long) {
    Thread.sleep(time)
  }

  val f = Future {
    sleep(Random.nextInt(500))
    42
  }
  println("before onComplete")
  f.onComplete {
    case Success(value) => println(s"Got the callback, meaning = $value")
    case Failure(e) => e.printStackTrace()
  }
  // do the rest of your work
  println("A ...")
  sleep(100)

  println("B ...")
  sleep(100)

  println("C ...")
  sleep(100)

  println("D ...")
  sleep(100)

  println("E ...")
  sleep(100)

  println("F ...")
  sleep(100)

  sleep(2000)
}
