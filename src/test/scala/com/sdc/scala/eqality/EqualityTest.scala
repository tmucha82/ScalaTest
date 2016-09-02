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

  test("Solution of Pitfall #1") {

    class Point(val x: Int, val y: Int) {

      def canEqual(other: Any): Boolean = other.isInstanceOf[Point]

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


}
