import scala.collection.immutable.IndexedSeq

def isPrime(n: Int): Boolean = {
  (2 until n).forall(n % _ != 0)
}

def getPrimesForPairs(n: Int): IndexedSeq[(Int, Int)] = {
  ((1 until n) flatMap (i => (1 until i) map (j => (i, j)))).filter { case (x, y) => isPrime(x + y) }
}

def getPrimesForPairsWithFor(n: Int): IndexedSeq[(Int, Int)] = {
  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)
}

def scalarProduct(xs: List[Double], ys: List[Double]): Double = xs.zip(ys).map { case (x, y) => x * y }.sum

def scalarProductWithFor(xs: List[Double], ys: List[Double]): Double = {
  (for ((i, j) <- xs.zip(ys)) yield i * j).sum
}

isPrime(2)
isPrime(4)
isPrime(5)
isPrime(7)

getPrimesForPairs(7)
getPrimesForPairsWithFor(7)

scalarProduct(List(1, 2, 3, 4), List(4, 3, 2, 1))
scalarProductWithFor(List(1, 2, 3, 4), List(4, 3, 2, 1))
