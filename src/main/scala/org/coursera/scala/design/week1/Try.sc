import scala.util.control.NonFatal

case class Success[T](x: T) extends Try[T]

case class Failure(exception: Throwable) extends Try[Nothing]

object Try {
  def apply[T](expr: => T): Try[T] = {
    try Success(expr)
    catch {
      case NonFatal(exc) => Failure(exc)
    }
  }
}

trait Try[+T] {
  def flatMap[U](f: T => Try[U]): Try[U] = this match {
    case Success(t) => try f(t) catch {
      case NonFatal(exc) => Failure(exc)
    }
    case fail: Failure => fail
  }

  def map[U](f: T => U): Try[U] = this match {
    case Success(t) => Try(f(t))
    case fail: Failure => fail
  }
}


Try(2 + 2)
Try(2 / 0)

for {
  x <- Try(2 + 2)
  y <- Try(0 / 2)
  z <- Try(3 * 0)
} yield (x, y, z)

for {
  x <- Try(2 + 2)
  y <- Try(2 / 0)
  z <- Try(3 * 0)
} yield (x, y, z)