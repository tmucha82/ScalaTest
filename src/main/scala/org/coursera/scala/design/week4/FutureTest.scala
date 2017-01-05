package org.coursera.scala.design.week4

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
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
  Await.result(f, Duration.Inf)

  //recover
  val divide = Future(6 / 0) recover { case e: ArithmeticException => 0 } // result: 0
  divide.onSuccess { case result => println(s"Result = $result") }

  //recoverWith
  val f2 = (x: Int, y: Int) => Future {
    x.max(y)
  }

  val divideTwo = (x: Int, y: Int) => Future(x / y) recoverWith { case e: ArithmeticException => f2(x, y) } // result: 0
  divideTwo(6,0).onSuccess { case result => println(s"Result2 with fail = $result") }
  divideTwo(6,2).onSuccess { case result => println(s"Result2 with OK = $result") }

  val divideTwoWithFallback = (x: Int, y: Int) => Future(x / y) fallbackTo { f2(x, y) } // result: 0
  divideTwoWithFallback(6,0).onSuccess { case result => println(s"Result3 with fail = $result") }
  divideTwoWithFallback(6,2).onSuccess { case result => println(s"Result3 with OK = $result") }


  val f3 = Future { sys.error("failed") }
  val g = Future { 5 }
  val h = f3 fallbackTo g
  val resultH = Await.result(h, Duration.Zero) // evaluates to 5
  println(resultH)
}
