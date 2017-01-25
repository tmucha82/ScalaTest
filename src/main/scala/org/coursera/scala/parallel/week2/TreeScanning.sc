import org.coursera.scala.parallel.common._

sealed abstract class Tree[A]

case class Leaf[A](a: A) extends Tree[A]

case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

sealed abstract class TreeRes[A] {
  val res: A
}

case class LeafRes[A](override val res: A) extends TreeRes[A]

case class NodeRes[A](l: TreeRes[A], override val res: A, r: TreeRes[A]) extends TreeRes[A]

//reducesRes but using parallelism
def upsweep[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
  case Leaf(v) => LeafRes(v)
  case Node(l, r) =>
    val (tL, tR) = parallel(upsweep(l, f), upsweep(r, f))
    NodeRes(tL, f(tL.res, tR.res), tR)
}
def downsweep[A](t: TreeRes[A], a0: A, f: (A, A) => A): Tree[A] = t match {
  case LeafRes(a) => Leaf(f(a0, a))
  case NodeRes(l, _, r) =>
    val (tL, tR) = parallel(downsweep[A](l, a0, f),
      downsweep[A](r, f(a0, l.res), f))
    Node(tL, tR)
}
def prepend[A](x: A, t: Tree[A]): Tree[A] = t match {
  case Leaf(v) => Node(Leaf(x), Leaf(v))
  case Node(l, r) => Node(prepend(x, l), r)
}

//finally
def scanLeft[A](t: Tree[A], element: A, f: (A, A) => A): Tree[A] = {
  val tRes = upsweep(t, f)
  val scan1 = downsweep(tRes, element, f)
  prepend(element, scan1)
}

val t1 = Node(Node(Leaf(1), Leaf(3)), Node(Leaf(8), Leaf(50)))
val plus = (x: Int, y: Int) => x + y
val treeRes = upsweep(t1, plus)
downsweep(treeRes, 100, plus)

scanLeft(t1, 100, plus)

