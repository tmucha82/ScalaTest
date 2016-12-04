def msort(list: List[Int]): List[Int] = {
  val n = list.length / 2
  if (n == 0) list
  else {
    /*
        def merge(xs: List[Int], ys: List[Int]): List[Int] = xs match {
          case Nil => ys
          case x :: xs1 =>
            ys match {
              case Nil => xs
              case y :: ys1 =>
                if (x < y) x :: merge(xs1, ys)
                else y :: merge(xs, ys1)
            }
        }
    */
    def merge(xs: List[Int], ys: List[Int]): List[Int] =
      (xs, ys) match {
        case (Nil, ys1) => ys1
        case (xs1, Nil) => xs1
        case (x :: xs1, y :: ys1) => if (x < y) x :: merge(xs1, ys) else y :: merge(xs, ys1)
      }
    val (first, last) = list.splitAt(n)
    merge(msort(first), msort(last))
  }
}

def msortParam[T](list: List[T])(lt: (T, T) => Boolean): List[T] = {
  val n = list.length / 2
  if (n == 0) list
  else {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, ys1) => ys1
        case (xs1, Nil) => xs1
        case (x :: xs1, y :: ys1) => if (lt(x, y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
      }
    val (first, last) = list.splitAt(n)
    merge(msortParam(first)(lt), msortParam(last)(lt))
  }
}

def msortOrder[T](list: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = list.length / 2
  if (n == 0) list
  else {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, ys1) => ys1
        case (xs1, Nil) => xs1
        case (x :: xs1, y :: ys1) => if (ord.lt(x, y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
      }
    val (first, last) = list.splitAt(n)
    merge(msortOrder(first), msortOrder(last))
  }
}

val list = List(3, 6, 13, 2, 1, 7, 9)
val fruits = List("plum", "apple", "pear")
msort(list)
msortParam(list)((x, y) => x < y)
msortParam(fruits)((x, y) => x.compareTo(y) < 0)
msortOrder(list)(new Ordering[Int] {
  override def compare(x: Int, y: Int): Int = if (x < y) -1 else if (x > y) 1 else 0
}) //but it is already there!!!
msortOrder(fruits)(new Ordering[String] {
  override def compare(x: String, y: String): Int = x.compareTo(y)
}) //but it is already there!!!

msortOrder(list)
msortOrder(fruits)
