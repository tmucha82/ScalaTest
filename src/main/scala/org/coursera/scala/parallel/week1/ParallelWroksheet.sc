//parallel - it is only for marking, it is not parallel really
def parallel[A, B](taskA: => A, taskB: => B): (A, B) = (taskA, taskB)

def power(x: Int, p: Double): Int = Math.exp(p * Math.log(Math.abs(x))).toInt

// this is how I would do that with functional style
def sumSegmentMine(array: Array[Int], p: Double, s: Int, t: Int): Int = {
  array.slice(s, t).foldLeft(0)((previous, next) => previous + power(next, p))
}

// yak - this is for parallel reason
def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int = {
  var i = s
  var sum: Int = 0
  while (i < t) {
    sum = sum + power(a(i), p)
    i = i + 1
  }
  sum
}
def pNorm(a: Array[Int], p: Double): Int = power(sumSegment(a, p, 0, a.length), 1 / p)

//sequential
def pNormTwoPartSeq(a: Array[Int], p: Double): Int = {
  val m = a.length / 2
  val (sum1, sum2) = (sumSegment(a, p, 0, m), sumSegment(a, p, m, a.length))
  power(sum1 + sum2, 1 / p)
}

def pNormTwoPartPar(a: Array[Int], p: Double): Int = {
  val m = a.length / 2
  val (sum1, sum2) = parallel(sumSegment(a, p, 0, m), sumSegment(a, p, m, a.length))
  power(sum1 + sum2, 1 / p)
}

def pNormFourPartPar(a: Array[Int], p: Double): Int = {
  val m1 = a.length / 4
  val m2 = a.length / 2
  val m3 = 3 * a.length / 4
  val ((sum1, sum2), (sum3, sum4)) = parallel(
    parallel(sumSegment(a, p, 0, m1), sumSegment(a, p, m1, m2)),
    parallel(sumSegment(a, p, m2, m3), sumSegment(a, p, m3, a.length))
  )
  power(sum1 + sum2 + sum3 + sum4, 1 / p)
}
//finally - divide to small pieces
// like sumSegment but parallel
// cool
val threshold = 2
def segmentRec(a: Array[Int], p: Double, s: Int, t: Int): Int = {
  if (t - s < threshold) sumSegment(a, p, s, t) // small segment: do it sequentially
  else {
    val m = s + (t - s) / 2
    val (sum1, sum2) = parallel(segmentRec(a, p, s, m), segmentRec(a, p, m, t))
    sum1 + sum2
  }
}

def pNormPar(a: Array[Int], p: Double): Int = {
  power(segmentRec(a, p, 0, a.length), 1 / p)
}


sumSegmentMine(Array(1, 2, 3, 4, 5, 6), 2, 2, 4)
sumSegment(Array(1, 2, 3, 4, 5, 6), 2, 2, 4)

pNorm(Array(1, 2, 3, 4, 5, 6), 2)
pNormTwoPartSeq(Array(1, 2, 3, 4, 5, 6), 2)
pNormTwoPartPar(Array(1, 2, 3, 4, 5, 6), 2)
pNormFourPartPar(Array(1, 2, 3, 4, 5, 6), 2)
pNormFourPartPar(Array(1, 2, 3, 4, 5, 6), 2)
pNormPar(Array(1, 2, 3, 4, 5, 6), 2)