trait MyBuilder[A, Repr] {
  def +=(elem: A): MyBuilder[A, Repr]

  def result: Repr
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
