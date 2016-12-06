package com.sdc.scala.eqality

import com.sdc.scala.rational.Rational
import org.scalatest.FunSuite

import scala.collection.mutable

class EqualityTest extends FunSuite {

  test("Pitfall #1: Defining equals with the wrong signature.") {

    class Point(val x: Int, val y: Int) {
      def equals(other: Point): Boolean =
        this.x == other.x && this.y == other.y
    }

    val p1, p2 = new Point(1, 2)
    val q = new Point(2, 3)
    assert(p1 equals p2)
    assert(!(p1 equals q))
    assert(!(p2 equals q))

    //However, trouble starts once you start putting points into a collection:
    val points = mutable.HashSet(p1)
    assert(points contains p1)
    // assert(points contains p2) - why false if p1 === p2
    //this is because...

    val p2a: Any = p2
    assert(!(p1 equals p2a)) //strange
    //it is because wrong way of override equals method
  }

  test("Solution of Pitfall #1 and Pitfall #2") {

    class Point(val x: Int, val y: Int) {

      def canEqual(other: Any): Boolean = other.isInstanceOf[Point]

      // A better definition, but still not perfect - instead equals for Point override equals for Any
      override def equals(other: Any): Boolean = other match {
        case that: Point =>
          (that canEqual this) &&
            x == that.x &&
            y == that.y
        case _ => false
      }

      override def hashCode(): Int = {
        val state = Seq(x, y)
        state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
      }
    }

    val p1, p2 = new Point(1, 2)
    val q = new Point(2, 3)
    assert(p1 equals p2)
    assert(!(p1 equals q))
    assert(!(p2 equals q))

    //However, trouble starts once you start putting points into a collection:
    val points: mutable.Set[Any] = mutable.HashSet(p1)
    assert(points contains p1)
    assert(points contains p2)

    val p2a: Any = p2
    assert(p1 equals p2a)
  }

  test("Solution of Pitfall #3") {

    //var instead of val - problematic
    class Point(var x: Int, var y: Int) {

      def canEqual(other: Any): Boolean = other.isInstanceOf[Point]

      // A better definition, but still not perfect - instead equals for Point override equals for Any
      override def equals(other: Any): Boolean = other match {
        case that: Point =>
          (that canEqual this) &&
            x == that.x &&
            y == that.y
        case _ => false
      }

      override def hashCode(): Int = {
        val state = Seq(x, y)
        state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
      }
    }

    val point = new Point(1, 2)
    val points: mutable.Set[Any] = mutable.HashSet(point)
    assert(points contains point)

    //Now, if you change a field in point, does the collection still contain the point?
    point.x += 1
    //!!! - when equals and hashCode depend on mutable state, it causes problems for potential user
    assert(!(points contains point))
  }

  test("Solution of Pitfall #4") {
    object Color extends Enumeration {
      val Red, Orange, Yellow, Green, Blue, Indigo, Violet = Value
    }

    class Point(val x: Int, val y: Int) {

      def canEqual(other: Any): Boolean = other.isInstanceOf[Point]

      // A better definition, but still not perfect - instead equals for Point override equals for Any
      override def equals(other: Any): Boolean = other match {
        case that: Point =>
          (that canEqual this) &&
            x == that.x &&
            y == that.y
        case _ => false
      }

      override def hashCode(): Int = {
        val state = Seq(x, y)
        state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
      }
    }

    class ColoredPoint(x: Int, y: Int, val color: Color.Value)
      extends Point(x, y) {
      // Problem: equals not symmetric

      override def equals(other: Any) = other match {
        case that: ColoredPoint =>
          this.color == that.color && super.equals(that)
        //we added new case
        case that: Point =>
          that.equals(this)
        case _ => false
      }
    }


    val point = new Point(1, 2)

    val coloredPoint = new ColoredPoint(1, 2, Color.Red)

    assert(point equals coloredPoint) //yep
    //assert(!(coloredPoint equals point)) //no - not symmetric, before adding case in equals
    assert(coloredPoint equals point) //yep, after change
    assert(mutable.HashSet[Point](point) contains coloredPoint) //yep
    //assert(!(mutable.HashSet[Point](coloredPoint) contains point)) //no - not symmetric, before adding case in equals
    assert(mutable.HashSet[Point](coloredPoint) contains point) //yep, after change

    //Now the problem is that the new relation is no longer transitive!

    val redPoint = new ColoredPoint(1, 2, Color.Red)
    val bluePoint = new ColoredPoint(1, 2, Color.Blue)

    assert(redPoint equals point)
    assert(point equals bluePoint)
    //but....
    assert(!(redPoint equals bluePoint))
  }

  test("Solution of Pitfall #4 part 2") {
    object Color extends Enumeration {
      val Red, Orange, Yellow, Green, Blue, Indigo, Violet = Value
    }

    class Point(val x: Int, val y: Int) {

      def canEqual(other: Any): Boolean = other.isInstanceOf[Point]

      // A better definition, but still not perfect - instead equals for Point override equals for Any
      override def equals(other: Any): Boolean = other match {
        case that: Point =>
          (that canEqual this) &&
            x == that.x &&
            y == that.y
        case _ => false
      }

      override def hashCode(): Int = {
        val state = Seq(x, y)
        state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
      }
    }

    class ColoredPoint(x: Int, y: Int, val color: Color.Value)
      extends Point(x, y) {

      override def canEqual(other: Any) = other.isInstanceOf[ColoredPoint]

      override def equals(other: Any) = other match {
        case that: ColoredPoint =>
          (that canEqual this) && this.color == that.color && super.equals(that)
        case _ => false
      }
    }

    val point = new Point(1, 2)
    val coloredPoint = new ColoredPoint(1, 2, Color.Red)
    val differentPoint = new Point(1, 1) {
      override val y = 2
    }

    //you should compare only the same classes
    assert(!(point equals coloredPoint)) //yep
    assert(!(coloredPoint equals point))
    assert(!(mutable.HashSet[Point](point) contains coloredPoint))
    assert(!(mutable.HashSet[Point](coloredPoint) contains point))

    //ss pAnon equal to p? The answer is no because the java.lang.Class objects associated with p and pAnon are different
    //assert(!(differentPoint equals point)) //sick!!
    assert(differentPoint equals point)

    val colorCollection = List(point)

    assert(colorCollection contains point)
    assert(!(colorCollection contains coloredPoint))
    assert(colorCollection contains differentPoint)
    //One potential criticism of the canEqual approach is that it violates the Liskov Substitution Principle (LSP)
  }

  test("Defining equality for parametrized types") {
    trait Tree[+T] {
      def element: T

      def left: Tree[T]

      def right: Tree[T]
    }

    object EmptyTree extends Tree[Nothing] {
      def element =
        throw new NoSuchElementException("EmptyTree.elem")

      def left =
        throw new NoSuchElementException("EmptyTree.left")

      def right =
        throw new NoSuchElementException("EmptyTree.right")
    }

    class Branch[+T](
                      val element: T,
                      val left: Tree[T],
                      val right: Tree[T]
                    ) extends Tree[T] {

      override def equals(other: Any) = other match {
        case that: Branch[_] => (that canEqual this) &&
          this.element == that.element &&
          this.left == that.left &&
          this.right == that.right
        case _ => false
      }

      override def hashCode: Int =
        41 * (
          41 * (
            41 + element.hashCode
            ) + left.hashCode
          ) + right.hashCode

      def irstCanEqual(other: Any) = other match {
        case that: Branch[_] => true
        case _ => false
      }

      //could be also
      def canEqual(other: Any) = other.isInstanceOf[Branch[_]]
    }


    val branch1 = new Branch[List[String]](Nil, EmptyTree, EmptyTree)
    val branch2 = new Branch[List[Int]](Nil, EmptyTree, EmptyTree)
    assert(branch1 == branch2)
    assert(branch1 equals branch2)
  }

  test("recipes for equals and hashCode") {
    val rational1 = new Rational(2,3)
    val rational2 = new Rational(2,3)
    assert(rational1 === rational2)
  }
}
