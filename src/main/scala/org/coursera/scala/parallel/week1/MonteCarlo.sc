import scala.util.Random

/**
  * Counts hits which would be in circle
  *
  * @param iteration number of all hits
  * @return hits which would be in circle
  */
def mcCount(iteration: Int): Int = {
  val randomX = new Random
  val randomY = new Random
  var hits = 0
  for (i <- 0 until iteration) {
    val x = randomX.nextDouble // in [0,1]
    val y = randomY.nextDouble // in [0,1]
    if (x * x + y * y < 1) hits = hits + 1
  }
  hits
}
def mcCountMine(iteration: Int): Int = {
  def isInCircle(x: Double, y: Double): Boolean = x * x + y * y < 1

  val (randomX, randomY) = (new Random, new Random)
  (for (i <- 0 until iteration; if isInCircle(randomX.nextDouble, randomY.nextDouble)) yield i).length
}
def monteCarloPiSeq(iteration: Int): Double = {
  4.0 * mcCountMine(iteration) / iteration
//  4.0 * mcCount(iteration) / iteration
}
monteCarloPiSeq(1000000)