//this is associative but because Double is floating point we need to be
// aware that this representation could do some differences.
// keep in mind that floating point are not fully associative
def f(u: Double, v: Double): Double = (u + v) / (1.0 + u * v)

def err(lst: List[Double]): Double = lst.reduceLeft(f) - lst.reduceRight(f)

def testAssoc: Double = {
  val r = new scala.util.Random
  val lst = List.fill(400)(r.nextDouble * 0.002)
  err(lst)
}

def testAssoc2: Double = {
  val r = new scala.util.Random
  val lst: List[Double] = List.fill(400)(r.nextInt)
  err(lst)
}

testAssoc
testAssoc2