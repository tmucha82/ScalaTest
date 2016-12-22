//method from functional package
def isPrime(n: Int): Boolean = {
  (2 until n) forall (x => n % x != 0)
}
isPrime(2)
isPrime(3)
isPrime(17)

//write method which check second prime number between 1000 and 10000
//this is not good implementation - use Stream
(1000 until 10000).filter(isPrime)(1)

// using Stream
(1000 until 10000).toStream.filter(isPrime)(1)
def listRange(lo: Int, hi: Int): List[Int] = {
  println(lo)
  if (lo >= hi) Nil
  else lo :: listRange(lo + 1, hi)
}
def streamRange(lo: Int, hi: Int): Stream[Int] = {
  println(lo)
  if (lo >= hi) Stream.empty
  else Stream.cons(lo, streamRange(lo + 1, hi))
}
listRange(1,10).take(2)
streamRange(1,10).take(2).toList
