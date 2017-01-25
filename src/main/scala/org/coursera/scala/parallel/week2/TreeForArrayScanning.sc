import org.coursera.scala.parallel.common._

sealed abstract class TreeResA[A] {
  val res: A
}

case class Leaf[A](from: Int, to: Int, override val res: A) extends TreeResA[A]

case class Node[A](l: TreeResA[A], override val res: A, r: TreeResA[A]) extends TreeResA[A]

def reduceSeg1[A](in: Array[A], start: Int, end: Int, element: A, f: (A, A) => A): A = {
  var res = element
  for (i <- start until end) {
    res = f(res, in(i))
  }
  res
}

def scanLeftSeg[A](in: Array[A], start: Int, end: Int, element: A, f: (A, A) => A, out: Array[A]): Array[A] = {
  if (start < end) {
    var i = start
    var a = element
    while (i < end) {
      a = f(a, in(i))
      i = i + 1
      out(i) = a
    }
  }
  out
}
val threshold = 2
def upSweep[A](in: Array[A], from: Int, to: Int, f: (A, A) => A): TreeResA[A] = {
  if (to - from < threshold)
    Leaf(from, to, reduceSeg1(in, from + 1, to, in(from), f))
  else {
    val mid = from + (to - from) / 2
    val (tL, tR) = parallel(upSweep(in, from, mid, f), upSweep(in, mid, to, f))
    Node(tL, f(tL.res, tR.res), tR)
  }
}
def downSweep[A](in: Array[A], element: A, f: (A, A) => A, treeRes: TreeResA[A], out: Array[A]): Array[A] = treeRes match {
  case Leaf(from, to, res) =>
    scanLeftSeg(in, from, to, element, f, out)
  case Node(l, _, r) =>
    val (_, _) = parallel(downSweep(in, element, f, l, out), downSweep(in, f(element, l.res), f, r, out))
    out
}
//finally
def scanLeft[A](inp: Array[A], element: A, f: (A, A) => A, out: Array[A]) = {
  val t = upSweep(inp, 0, inp.length, f)
  downSweep(inp, element, f, t, out) // fills out[1..inp.length]
  out(0) = element // prepends a0
}
val array = Array(1, 3, 8)
val result1 = new Array[Int](array.length + 1)
val plus = (x: Int, y: Int) => x + y
upSweep(array, 0, array.length, plus)
downSweep(array, 100, plus, upSweep(array, 0, array.length, plus), result1) // fills out[1..inp.length]
scanLeft(array, 100, plus, result1)
result1 //should be (100, 101, 104, 112)
