def last[T](list: List[T]): T = list match {
  case List() => throw new NoSuchElementException
  case List(x) => x
  case x :: xs => last(xs)
}

def init[T](list: List[T]): List[T] = list match {
  case List() => throw new NoSuchElementException
  case List(x) => Nil
  case x :: xs => x :: init(xs)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](list: List[T]): List[T] = list match {
  case List() => list
  case x :: xs => reverse(xs) ::: List(x)
}

def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
  case List() => throw new NoSuchElementException
  case y :: ys => if (n == 0) ys else y :: removeAt(n - 1, ys)
}

def differentRemoveAt[T](n: Int, xs: List[T]): List[T] = xs.take(n) ::: xs.drop(n + 1)

val list = List(1, 2, 3, 4, 5)
last(list)
init(list)
val another = List(5, 4, 3, 2, 1)
concat(list, another)
reverse(list)

removeAt(1, List('a', 'b', 'c', 'd')) // List(a, c, d)
differentRemoveAt(1, List('a', 'b', 'c', 'd')) // List(a, c, d)
