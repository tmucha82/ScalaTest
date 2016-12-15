def sum1(xs: List[Int]): Int = (0 :: xs) reduceLeft ((x, y) => x + y) //... or shorter
def sum2(xs: List[Int]): Int = (0 :: xs) reduceLeft (_ + _) //or even shorter using fold left
def sum3(xs: List[Int]): Int = xs.foldLeft(0)(_ + _) //or.... just
def sum4(xs: List[Int]): Int = xs.sum

def product1(xs: List[Int]): Int = (1 :: xs) reduceLeft ((x, y) => x * y) // ... or shorter
def product2(xs: List[Int]): Int = (1 :: xs) reduceLeft (_ * _) //or even shorter using fold left
def product3(xs: List[Int]): Int = xs.foldLeft(1)(_ * _) //or... just
def product4(xs: List[Int]): Int = xs.product


def concat[T](xs: List[T], ys: List[T]): List[T] = {
  xs.foldRight(ys)(_ :: _)
}
def concat2[T](xs: List[T], ys: List[T]): List[T] = {
  //  xs.foldLeft(ys)(_ :: _) //sorry fold left does not work - :: is for y:: List() and no for List():: y
  throw new IllegalStateException
}
val list = List(2, 4, 1, 7, 9)
sum1(list)
sum2(list)
sum3(list)
sum4(list)
product1(list)
product2(list)
product3(list)
product4(list)

val list1 = List(0, 2, 4, 6, 8)
val list2 = List(1, 3, 5, 7, 9)
concat(list1, list2)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]()) (f(_) :: _)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0) ((_, acc) => 1 + acc)

mapFun(list1, (x: Int) => x + 1)
mapFun(list2, (x: Int) => x - 1)

lengthFun(list1)
lengthFun(list2)
