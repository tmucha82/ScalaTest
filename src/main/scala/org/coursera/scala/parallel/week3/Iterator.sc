trait MyIterator[T] {
  def hasNext: Boolean

  def next(): T

  def foldLeft[S](z: S)(f: (S, T) => S): S = {
    var result = z
    while (hasNext) result = f(result, next())
    result
  }
}

case class MyRange(val start: Int, val end: Int) {

  def iterator = new MyIterator[Int] {
    var cursor = start

    override def hasNext: Boolean = cursor < end

    override def next(): Int = {
      if (hasNext) {
        val current = cursor
        cursor = cursor + 1
        return current
      }
      throw new NoSuchElementException()
    }
  }
}

// ... now in works
//val iterator = MyRange(10, 15).iterator
//iterator.foldLeft(0)(_ + _)




