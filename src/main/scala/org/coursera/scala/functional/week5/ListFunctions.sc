def squareList1(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => y * y :: squareList1(ys)
  }

def squareList2(xs: List[Int]): List[Int] =
  xs map (x => x * x)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs.span(y => y == x)
    first :: pack(rest)
}

def encode[T](xs: List[T]): List[(T, Int)] = {
  pack(xs) map (list => (list.head, list.length))
}
val list = List(1, 2, 3, 4, 5)
squareList1(list)
squareList2(list)
val nums = List(3, -6, 13, -2, 1, 7, 9)
val fruits = List("plum", "apple", "pear")
nums filter (x => x > 0)
nums filterNot (x => x > 0)
nums partition (x => x > 0)
nums takeWhile (x => x > 0)
nums dropWhile (x => x > 0)
nums span (x => x > 0)

pack(List("a", "a", "a", "b", "c", "c", "a")) // should give: List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))
encode(List("a", "a", "a", "b", "c", "c", "a")) // List(("a", 3), ("b", 1), ("c", 2), ("a", 1))