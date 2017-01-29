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

trait MyBuilder[A, Repr] {
  def +=(elem: A): MyBuilder[A, Repr]

  def result: Repr
}

trait MyCombiner[A, Repr] extends MyBuilder[A, Repr] {
  def combine(that: MyCombiner[A, Repr]): MyCombiner[A, Repr]
}

trait MyTraversable[T] {
  def foreach(f: T => Unit): Unit

  def newBuilder: MyBuilder[T, MyTraversable[T]]

  def filter(p: T => Boolean): MyTraversable[T] = {
    val b = newBuilder
    foreach(element => {
      if (p(element)) b += element
    })
    b.result
  }
}

trait ParTraversable[T] extends MyTraversable[T] with MySplitter[T] {

  override def split: Seq[ParTraversable[T]]
  def newCombiner: MyCombiner[T, ParTraversable[T]]

  def parFilter(p: T => Boolean): MyTraversable[T] = {
/*
    if (remaining < threshold) filter(p)
    else {
      val children: Seq[ForkJoinTask[MyTraversable[T]]] = for (child <- split) yield task {
        child.parFilter(p)
      }
    }
    val c = newCombiner
    for (x <- this) if (p(x)) c += x
    c.result
*/
    ???
  }
}
