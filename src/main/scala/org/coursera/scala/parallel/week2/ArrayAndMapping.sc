import org.coursera.scala.parallel.common._

def mapSegmentSeq[A, B](in: Array[A], out: Array[B], start: Int, end: Int, f: A => B) = {
  for(i <- start until end) out(i) = f(in(i))
}

def mapSegmentPar[A, B](in: Array[A], out: Array[B], start: Int, end: Int, f: A => B)(threshold: Int = 2) = {
  // Writes to out(i) for left <= i <= right-1
  if (end-start < threshold)
    mapSegmentSeq(in, out, start, end, f)
  else {
    val mid = start + (end - start)/2
    parallel(
      mapSegmentPar(in, out, start, mid, f),
      mapSegmentPar(in, out, mid, end, f)
    )
  }
}

def mapSeq[A, B](in: Array[A], out: Array[B], f: A => B) = mapSegmentSeq(in, out, 0, in.length, f)
def mapPar[A, B](in: Array[A], out: Array[B], f: A => B) = mapSegmentPar(in, out, 0, in.length, f)


val array = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
var result = new Array[Int](array.length)

val f = (x: Int) => x * x
mapSeq(array, result, f)
result

result = new Array(array.length)
mapPar(array, result, f)
