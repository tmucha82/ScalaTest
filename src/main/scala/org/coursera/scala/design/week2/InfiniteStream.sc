//all integers starts from
def from(n: Int): Stream[Int] = n #:: from(n + 1)
//all natural numbers
val nats = from(1)
//all multiplies by 4
val m4s = nats.map(_ * 4)
m4s.take(100).toList


//sieve to calculate prime numbers
def sieve(numbers: Stream[Int]): Stream[Int] =
  numbers.head #:: sieve(numbers.tail.filter(_ % numbers.head != 0))

val primes = sieve(from(2))

primes.take(20).toList

def abs(x: Double) = if (x < 0) -x else x
def isGoodEnough(guess: Double, x: Double) = abs(guess * guess - x) < 0.0001 * x

def sqrt(x: Double) = {

  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess))
  def improve(guess: Double) = (guess + x / guess) / 2

  sqrtIter(1.0)
}

//sqrt
def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2

  lazy val guesses: Stream[Double] = 1 #:: guesses.map(improve)
  guesses
}

sqrtStream(2)(20)
sqrtStream(2).filter(isGoodEnough(_, 2))

