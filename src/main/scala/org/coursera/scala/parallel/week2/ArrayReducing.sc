import org.coursera.scala.parallel.common._

def reduceSegmentSeq[A](in: Array[A], start: Int, end: Int, f: (A, A) => A): A = {
  var res = in(start)
  for (i <- start + 1 until end) {
    res = f(res, in(i))
  }
  res
}

def reduceSegmentPar[A](in: Array[A], start: Int, end: Int, f: (A, A) => A)(threshold: Int = 2): A = {
  if (end - start < threshold) {
    reduceSegmentSeq(in, start, end, f)
  } else {
    val mid = start + (end - start) / 2
    val (a1, a2) = parallel(
      reduceSegmentPar(in, start, mid, f)(threshold),
      reduceSegmentPar(in, mid, end, f)(threshold)
    )
    f(a1, a2)
  }
}
def reduceSeq[A](in: Array[A], f: (A, A) => A): A = reduceSegmentSeq(in, 0, in.length, f)
def reducePar[A](in: Array[A], f: (A, A) => A): A = reduceSegmentPar(in, 0, in.length, f)(2)

reduceSeq(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), (x: Int, y: Int) => x + y)
reducePar(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), (x: Int, y: Int) => x + y)

