package com.sdc.scala.eqality

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
      extends Point(x, y) { // Problem: equals not symmetric

      override def equals(other: Any) = other match {
        case that: ColoredPoint =>
          this.color == that.color && super.equals(that)
        case _ => false
      }
    }


    val point = new Point(1, 2)

    val coloredPoint = new ColoredPoint(1, 2, Color.Red)

    assert(point equals coloredPoint) //yep
    assert(!(coloredPoint equals point)) //no - not symmetric
    assert(mutable.HashSet[Point](point) contains coloredPoint) //yep
    assert(!(mutable.HashSet[Point](coloredPoint) contains point)) //no - not symmetric
    //what we can do
  }
}
