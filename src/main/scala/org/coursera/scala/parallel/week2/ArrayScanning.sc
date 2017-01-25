import org.coursera.scala.parallel.common._

def scanLeftSeq[A](in: Array[A], element: A, f: (A, A) => A, out: Array[A]) = {
  out(0) = element
  for (i <- in.indices) {
    out(i + 1) = f(out(i), in(i))
  }
}
// how to do that parallel - using reduce and map?
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

def mapSegmentSeq[A, B](in: Array[A], out: Array[B], start: Int, end: Int, f: (Int, A) => B): Array[B] = {
  for (i <- start until end) out(i) = f(i, in(i))
  out
}

def mapSegmentPar[A, B](in: Array[A], out: Array[B], start: Int, end: Int, f: (Int, A) => B)(threshold: Int = 2): Array[B] = {
  // Writes to out(i) for left <= i <= right-1
  if (end - start < threshold)
    mapSegmentSeq(in, out, start, end, f)
  else {
    val mid = start + (end - start) / 2
    parallel(
      mapSegmentPar(in, out, start, mid, f)(threshold),
      mapSegmentPar(in, out, mid, end, f)(threshold)
    )
    out
  }
}


//... here it is
def scanLeftPar[A](in: Array[A], element: A, f: (A, A) => A, out: Array[A]) = {
  val fi = (i: Int, value: A) => f(reduceSegmentPar(in, 0, i, f)(2), element)
  mapSegmentPar(in, out, 0, in.length, fi)(2)
  val last = in.length - 1
  out(last + 1) = f(out(last), in(last))
}




val array = Array(1, 3, 8)
val result1 = new Array[Int](array.length + 1)
scanLeftSeq(array, 100, (x: Int, y: Int) => x + y, result1)
result1 //should be (100, 101, 104, 112)

val result2 = new Array[Int](array.length + 1)
scanLeftPar(array, 100, (x: Int, y: Int) => x + y, result2)
result2
