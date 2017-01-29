import java.util.concurrent.ForkJoinTask

import org.coursera.scala.parallel.common._

trait MyIterator[T] {
  def hasNext: Boolean

  def next(): T

  def foldLeft[S](z: S)(f: (S, T) => S): S = {
    var result = z
    while (hasNext) result = f(result, next())
    result
  }
}


trait MySplitter[T] extends MyIterator[T] {
  val threshold = 10

  def split: Seq[MySplitter[T]]

  def remaining: Int

  def fold(z: T)(f: (T, T) => T): T = {
    if (remaining < threshold) foldLeft(z)(f)
    else {
      val children: Seq[ForkJoinTask[T]] = split.map(x => task {
        x.fold(z)(f)
      })
      children.map(_.join()).foldLeft(z)(f)
    }
  }
}