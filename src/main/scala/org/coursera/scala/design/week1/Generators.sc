
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

def booleans: Generator[Boolean] = for (bool <- integers) yield bool > 0
def pairs[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = for (first <- t; second <- u) yield (first, second)
def single[T](x: T): Generator[T] = new Generator[T] {
  override def generate: T = x
}
def choose(lo: Int, hi: Int): Generator[Int] = for (x <- integers) yield lo + x % (hi - lo)
def oneOf[T](xs: T*): Generator[T] = for (x <- choose(0, xs.length)) yield xs(x)
def emptyList = single(Nil)
def nonEmptyList = for {
  head <- integers
  tail <- lists
} yield head :: tail

def lists: Generator[List[Int]] = for {
  isEmpty <- booleans
  list <- if (isEmpty) emptyList else nonEmptyList
} yield list


trait Tree
case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree

def trees: Generator[Tree] = for {
  isLeaf <- booleans
  tree <- if (isLeaf) leafs else inners
} yield tree

def leafs: Generator[Tree] = for {
  x <- integers
} yield Leaf(x)

def inners: Generator[Tree] = for {
  treeLeft <- trees
  treeRight <- trees
} yield Inner(treeLeft, treeRight)
integers.generate
booleans.generate
pairs(integers, integers).generate
single(3).generate
choose(3, 10).generate
oneOf("X", "Y", "Z").generate
lists.generate
trees.generate
