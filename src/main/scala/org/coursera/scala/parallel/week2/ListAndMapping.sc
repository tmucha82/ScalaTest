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

val list = List(0, 1, 2, 3, 4)
val f = (x: Int) => x * x
mapSeq(list, f)
mapPar(list, f)