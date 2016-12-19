
import scala.util.Random

trait Generator[+T] {
  self =>

  def generate: T

  def map[U](f: T => U): Generator[U] with Object = new Generator[U] {
    override def generate: U = f(self.generate)
  }

  def flatMap[U](f: T => Generator[U]): Generator[U] = new Generator[U] {
    override def generate: U = f(self.generate).generate
  }
}

val integers = new Generator[Int] {
  val random = new Random

  override def generate: Int = random.nextInt
}

def boolean: Generator[Boolean] = for (bool <- integers) yield bool > 0
def pairs[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = for (first <- t; second <- u) yield (first, second)
//def single

integers.generate
boolean.generate
pairs(integers, integers).generate
