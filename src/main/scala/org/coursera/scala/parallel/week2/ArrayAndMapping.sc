def mapSegmentSeq[A, B](in: Array[A], out: Array[B], start: Int, end: Int, f: A => B) = {
  ???
}

def mapSegmentPar[A, B](in: Array[A], out: Array[B], start: Int, end: Int, f: A => B) = {
  ???
}

def mapSeq[A, B](in: Array[A], out: Array[B], f: A => B) = mapSegmentSeq(in, out, 0, in.length, f)
def mapPar[A, B](in: Array[A], out: Array[B], f: A => B) = mapSegmentPar(in, out, 0, in.length, f)


val array = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
var result = new Array[Int](array.length)

val f = (x: Int) => x * x
mapSeq(array, result, f)

result = new Array(array.length)
mapPar(array, result, f)
