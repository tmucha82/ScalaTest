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

val list = List(3, 6, 13, 2, 1, 7, 9)
msort(list)