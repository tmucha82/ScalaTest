def abs(x: Double) = if (x < 0) -x else x

def sqrt(x: Double) = {
  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))
  def isGoodEnough(guess: Double) = abs(guess * guess - x) < 0.0001 * x
  def improve(guess: Double) = (guess + x / guess) / 2

  sqrtIter(1.0)
}
sqrt(2)
sqrt(4)
sqrt(1e-6)
sqrt(1e60)

val x = 0

def f(y: Int) = y + 1

val result = {
  val x = f(3)
  x * x
} + x