import org.coursera.scala.parallel.common._

def mapSeq[A, B](list: List[A], f: A => B): List[B] = list match {
  case Nil => Nil
  case head :: tail => f(head) :: mapSeq(tail, f)
}

//different approach (finding middle element) is not good choice - list is not ideal ofr such parallelism
def mapPar[A, B](list: List[A], f: A => B): List[B] = list match {
  case Nil => Nil
  case head :: tail =>
    val (a, b) = parallel(f(head), mapPar(tail, f))
    a :: b
}

def map[A, B](list: List[A], f: A => B): List[B] = mapPar(list, f)

def reduce[A](list: List[A], f: (A, A) => A): A = list match {
  case List(x) => x
  case x :: xs => f(x, reduce(xs, f))
}

val list = List(0, 1, 2, 3, 4)
val f = (x: Int) => x * x
mapSeq(list, f)
mapPar(list, f)

reduce(list, (x: Int, y: Int) => x + y)

// array norm using map and reduce:
val p = 2
reduce(map(list, (x: Int) => Math.pow(Math.abs(x), p).toInt), (x: Int, y: Int) => x + y)

// avarage
val list2 = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
val sum = reduce(list2, (x: Int, y: Int) => x + y)
val length = reduce(map(list2, (x: Int) => 1), (x: Int, y: Int) => x + y) //?!?!?!? - why just simply use length

val avg = sum.toDouble / length.toDouble

//can we do it using single reduce
val f2 = (x: (Int, Int), y: (Int, Int)) => (x._1 + y._1, x._2 + y._2)
val (sum2, length2) = reduce(map(list2, (x: Int) => (x, 1)), f2)
val avg2 = sum2.toDouble / length2.toDouble
