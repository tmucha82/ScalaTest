import org.coursera.scala.parallel.common._


sealed abstract class Tree[A]

case class Leaf[A](value: A) extends Tree[A]

case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def reduceSeq[A](t: Tree[A], f: (A, A) => A): A = t match {
  case Leaf(v) => v
  case Node(l, r) => f(reduceSeq(l, f), reduceSeq(r, f)) // Node -> f
}

def reducePar[A](t: Tree[A], f: (A, A) => A): A = t match {
  case Leaf(v) => v
  case Node(l, r) =>
    val (a, b) = parallel(reducePar(l, f), reducePar(r, f))
    f(a, b) // Node -> f
}

def map[A, B](t: Tree[A], f: A => B): Tree[B] = t match {
  case Leaf(v) => Leaf(f(v))
  case Node(l, r) => Node(map(l, f), map(r, f))
}

def tree = Node(Leaf(1), Node(Leaf(3), Leaf(8)))
def fMinus = (x: Int, y: Int) => x - y
reduceSeq(tree, fMinus) // 6
reducePar(tree, fMinus) // 6

def toList[A](t: Tree[A]): List[A] = t match {
  case Leaf(v) => List(v)
  case Node(l, r) => toList(l) ++ toList(r)
}

//Can you express toList using map and reduce?
def toList2[A](t: Tree[A]): List[A] = {
  reducePar(map(t, (x: A) => List(x)), (x: List[A], y: List[A]) => x ++ y)
}
toList(tree)
toList2(tree)
