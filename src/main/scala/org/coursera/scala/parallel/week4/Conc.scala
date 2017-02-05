package org.coursera.scala.parallel.week4

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

sealed trait Conc[+T] {
  def height: Int

  def size: Int

  def left: Conc[T] = Empty

  def right: NormalConc[T] = Empty

  def concat[U >: T](that: Conc[U]): Conc[U] = {
    Conc.concat(this, that)
  }

  def <>[U >: T](that: Conc[U]): Conc[U] = {
    concat(that)
  }

  def appendLeaf[U >: T](that: Leaf[U]): Conc[U] = {
    Conc.appendLeaf(this, that)
  }

  def normalize: NormalConc[T] = {
    Conc.normalize(this)
  }
}

sealed trait NormalConc[+T] extends Conc[T] {
  override val left: NormalConc[T] = Empty
}

object Conc {
  def traverse[T: ClassTag](tree: Conc[T]): Array[T] = {
    def loop(tree: NormalConc[T]): ArrayBuffer[T] = {
      tree match {
        case <>(left, right) => loop(left) ++ loop(right)
        case Chunk(arr, size) => ArrayBuffer(arr.slice(0, size): _*)
        case Singleton(value) => ArrayBuffer(value)
        case Empty => ArrayBuffer()
      }
    }
    loop(tree.normalize).toArray
  }

  private def concatNormal[T](x: NormalConc[T], y: NormalConc[T]): NormalConc[T] = {
    val diff = x.height - y.height
    if (-1 <= diff && diff <= 1) {
      <>(x, y)
    } else if (diff > 1) {
      if (x.left.height >= x.right.height) {
        val nr = concatNormal(x.right, y)
        <>(x.left, nr)
      } else {
        val z = concatNormal(x.right.right, y)
        if (z.height == x.height - 3) {
          val nr = <>(x.right.left, z)
          concatNormal(x.left, nr)
        } else {
          val nl = concatNormal(x.left, x.right.left)
          <>(nl, z)
        }
      }
    } else {
      if (y.left.height <= x.right.height) {
        val nl = concatNormal(x, y.left)
        <>(nl, y.right)
      } else {
        val z = concatNormal(x, y.left.left)
        if (z.height == y.height - 3) {
          val nl = <>(z, y.left.right)
          <>(nl, y.right)
        } else {
          val nr = <>(y.left.right, y.right)
          <>(z, nr)
        }
      }
    }
  }

  def concat[T](left: Conc[T], right: Conc[T]): Conc[T] = {
    val (x, y) = (left.normalize, right.normalize)
    concatNormal(x, y)
  }

  @tailrec
  def normalize[T](tree: Conc[T]): NormalConc[T] = {
    tree match {
      case tree: NormalConc[T] => tree
      case tree@Append(Append(ll, lr), r) => normalize(Append(ll, concatNormal(lr, r)))
      case tree@Append(l: NormalConc[T], r) => concatNormal(l, r)
    }
  }

  def appendLeaf[T](tree: Conc[T], leaf: Leaf[T]): Conc[T] = {
    @tailrec
    def loop(x: Append[T], y: NormalConc[T]): Conc[T] = {
      if (x.right.height > y.height) {
        Append(x, y)
      } else {
        assert(x.right.height == y.height)
        val z = <>(x.right, y)
        x.left match {
          case xl: Append[T] => loop(xl, z)
          case xl: NormalConc[T] if xl.height == z.height => <>(xl, z)
          case xl: NormalConc[T] => Append(xl, z)
        }
      }
    }
    tree match {
      case Empty => leaf
      case x: Leaf[T] => <>(x, leaf)
      case x: <>[T] => Append(x, leaf)
      case x: Append[T] => loop(x, leaf)
    }
  }
}

object Empty extends NormalConc[Nothing] {
  override val height: Int = 0
  override val size: Int = 0
}

sealed trait Leaf[+T] extends NormalConc[T] {
  override val height = 0
}

case class Singleton[+T](value: T) extends Leaf[T] {
  override val size = 1
}

case class Chunk[T](array: Array[T], override val size: Int) extends Leaf[T]

case class <>[+T](override val left: NormalConc[T], override val right: NormalConc[T]) extends NormalConc[T] {
  assert(
    math.abs(left.height - right.height) < 2,
    "Difference between the height of the" + "left and right subtree of <> must be less than 2")

  override val height: Int = math.max(left.height, right.height) + 1
  override val size: Int = left.size + right.size
}

case class Append[+T](override val left: Conc[T], override val right: NormalConc[T]) extends Conc[T] {
  override val height: Int = math.max(left.height, right.height) + 1
  override val size: Int = left.size + right.size
}