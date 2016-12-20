import scala.util.control.NonFatal

trait Try[+T]

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

Try(2+2)
Try(2/0)