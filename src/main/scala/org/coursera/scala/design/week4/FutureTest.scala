package org.coursera.scala.design.week4

import scala.concurrent.duration.Duration
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure, Random}

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
  divideTwo(6, 0).onSuccess { case result => println(s"Result2 with fail = $result") }
  divideTwo(6, 2).onSuccess { case result => println(s"Result2 with OK = $result") }

  val divideTwoWithFallback = (x: Int, y: Int) => Future(x / y) fallbackTo {
    f2(x, y)
  } // result: 0
  divideTwoWithFallback(6, 0).onSuccess { case result => println(s"Result3 with fail = $result") }
  divideTwoWithFallback(6, 2).onSuccess { case result => println(s"Result3 with OK = $result") }


  val f3 = Future {
    sys.error("failed")
  }
  val g = Future {
    5
  }
  val h = f3 fallbackTo g
  val resultH = Await.result(h, Duration.Zero) // evaluates to 5
  println(resultH)

  // understanding flatMap of Future - how it looks like
  trait TMFuture[T] extends Future[T] {
    self =>

    def flatMap[S](f: T => Future[S]): Future[S] = {
      new Future[S] {

        override def onComplete[U](callback: (Try[S]) => U)(implicit executor: ExecutionContext): Unit = {
          self.onComplete {
            case Success(result) => f(result).onComplete(callback)
            case Failure(ex) => callback(Failure(ex))
          }
        }

        override def isCompleted: Boolean = ???

        override def value: Option[Try[S]] = ???

        override def result(atMost: Duration)(implicit permit: CanAwait): S = ???

        override def ready(atMost: Duration)(implicit permit: CanAwait): this.type = ???
      }
    }

  }

  /**
    * Simply way to retry until it is successful
    *
    * @param noTimes number of times we try to do something
    * @param block   future to execute n times
    * @tparam T template
    * @return future to be OK
    */
  def retry[T](noTimes: Int)(block: => Future[T]): Future[T] = {
    if (noTimes == 0) {
      println("Sorry")
      Future.failed(new Exception("Sorry"))
    } else {
      println("Next")
      block.fallbackTo(retry(noTimes - 1)(block))
    }
  }

  /**
    * Of course we cour do without recursion---> foldLef, foldRight
    */
  def retryTwo[T](noTimes: Int)(block: => Future[T]): Future[T] = {
    val attempts = (1 to noTimes).map(attempt => () => block) //call by name
    val startElement = Future.failed(new Exception("Sorry"))
    attempts.foldLeft(startElement){case (attempt, future) => attempt.recoverWith{ case _ => future()}}

  }

  val futureWithRandomResult = Future {
    sleep(Random.nextInt(5000))
    Random.nextInt()
  }

  val resultFuture = retry(4)(futureWithRandomResult)
  resultFuture.onComplete {
    case Success(result) => println(s"Result of future is $result")
    case Failure(ex) => println(s"Problem with future = ${ex.getMessage}")
  }

  Await.result(resultFuture, Duration.Inf)

}
