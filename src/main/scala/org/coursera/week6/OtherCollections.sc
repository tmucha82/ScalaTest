val xs = Array(1, 2, 3, 44)
xs map (x => x * 2)

val s = "Hello World"
s filter (c => c.isUpper)

s exists (c => c.isUpper)
s forall (c => c.isUpper)

val list = List(1, 2, 3) zip s
list.unzip

s.flatMap(c => List('.', c))
s.map(c => List('.', c))

xs.sum
xs.max

//I want to list all combinations of numbers x and y where x is drawn from one interval. Let's say from 1 to M and y is drawn from another
val M = 6
val N = 4
(1 to M) map (x => (1 to N) map (y => (x, y)))
(1 to M) flatMap (x => (1 to N) map (y => (x, y)))
def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double = {
  ((xs zip ys) map (pair => pair._1 * pair._2)).sum
}

def anotherScalarProduct(xs: Vector[Double], ys: Vector[Double]): Double = {
  ((xs zip ys) map { case (x, y) => x * y }).sum
}

val vector1: Vector[Double] = Vector(1, 2, 3, 4)
val vector2: Vector[Double] = Vector(4, 3, 2, 1)

scalarProduct(vector1, vector2)
anotherScalarProduct(vector1, vector2)


def isPrime(n: Int):Boolean = {
  (2 until n) forall(x => n % x != 0)
}

isPrime(2)
isPrime(3)
isPrime(4)
isPrime(5)
isPrime(6)
isPrime(7)
isPrime(41)
isPrime(40)