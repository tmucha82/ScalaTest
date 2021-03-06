package org.coursera.scala.parallel.week4.assignment.barneshut

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.collection._
import scala.collection.parallel._
import scala.math._

@RunWith(classOf[JUnitRunner])
class BarnesHutSuite extends FunSuite {

  // test cases for quad tree

  import FloatOps._

  test("Empty: center of mass should be the center of the cell") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  test("Empty: mass should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  test("Empty: total should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  test("Leaf with 1 body") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))

    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Fork with 4 empty quadrants") {
    val nw = Empty(1f, 1f, 0f)
    val ne = Empty(3f, 1f, 0f)
    val sw = Empty(1f, 3f, 0f)
    val se = Empty(3f, 3f, 0f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX === 1)
    assert(quad.centerY === 1)
    assert(quad.mass === 0f)
    assert(quad.massX === 1f)
    assert(quad.massY === 1f)
    assert(quad.total === 0)
  }

  test("Fork with 3 empty quadrants and 1 leaf (nw)") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Empty.insert(b) should return a Leaf with only that body") {
    val quad = Empty(51f, 46.3f, 5f)
    val b = new Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 51f, s"$centerX should be 51f")
        assert(centerY == 46.3f, s"$centerY should be 46.3f")
        assert(size == 5f, s"$size should be 5f")
        assert(bodies == Seq(b), s"$bodies should contain only the inserted body")
      case _ =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
    }
  }

  test("Fork.insert(b) should return Fork with one body NW") {
    val quad = Fork(Empty(9f, 11f, 2f), Empty(11f, 11f, 2f), Empty(9f, 13f, 2f), Empty(11f, 13f, 2f))
    val b = new Body(3f, 9.5f, 11.5f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Fork(nw, ne, sw, se) =>
        assert(nw.total == 1, "NW of given Fork should have one body")
        assert(ne.total == 0, "NE of given Fork should be empty")
        assert(sw.total == 0, "SW of given Fork should be empty")
        assert(se.total == 0, "SE of given Fork should be empty")
      case _ =>
        fail("Fork.insert() should have returned a Fork, was $inserted")
    }
    assert(inserted.centerX == 10f, s"${inserted.centerX} should be 10f")
    assert(inserted.centerY == 12f, s"${inserted.centerY} should be 12f")
    assert(inserted.mass == b.mass, s"${inserted.mass} should be ${b.mass}")
    assert(inserted.massX == b.x, s"${inserted.massX} should be ${b.x}")
    assert(inserted.massY == b.y, s"${inserted.massY} should be ${b.y}")
    assert(inserted.total == 1, s"${inserted.total} should be 1")
  }

  test("Fork.insert(b) should return Fork with one body NE") {
    val quad = Fork(Empty(9f, 11f, 2f), Empty(11f, 11f, 2f), Empty(9f, 13f, 2f), Empty(11f, 13f, 2f))
    val b = new Body(3f, 10.5f, 10.5f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Fork(nw, ne, sw, se) =>
        assert(nw.total == 0, "NW of given Fork should be empty")
        assert(ne.total == 1, "NE of given Fork should have one body")
        assert(sw.total == 0, "SW of given Fork should be empty")
        assert(se.total == 0, "SE of given Fork should be empty")
      case _ =>
        fail("Fork.insert() should have returned a Fork, was $inserted")
    }
    assert(inserted.centerX == 10f, s"${inserted.centerX} should be 10f")
    assert(inserted.centerY == 12f, s"${inserted.centerY} should be 12f")
    assert(inserted.mass == b.mass, s"${inserted.mass} should be ${b.mass}")
    assert(inserted.massX == b.x, s"${inserted.massX} should be ${b.x}")
    assert(inserted.massY == b.y, s"${inserted.massY} should be ${b.y}")
    assert(inserted.total == 1, s"${inserted.total} should be 1")
  }

  test("Fork.insert(b) should return Fork with one body SW") {
    val quad = Fork(Empty(9f, 11f, 2f), Empty(11f, 11f, 2f), Empty(9f, 13f, 2f), Empty(11f, 13f, 2f))
    val b = new Body(3f, 8.5f, 12.5f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Fork(nw, ne, sw, se) =>
        assert(nw.total == 0, "NW of given Fork should be empty")
        assert(ne.total == 0, "NE of given Fork should be empty")
        assert(sw.total == 1, "SW of given Fork should have one body")
        assert(se.total == 0, "SE of given Fork should be empty")
      case _ =>
        fail("Fork.insert() should have returned a Fork, was $inserted")
    }
    assert(inserted.centerX == 10f, s"${inserted.centerX} should be 10f")
    assert(inserted.centerY == 12f, s"${inserted.centerY} should be 12f")
    assert(inserted.mass == b.mass, s"${inserted.mass} should be ${b.mass}")
    assert(inserted.massX == b.x, s"${inserted.massX} should be ${b.x}")
    assert(inserted.massY == b.y, s"${inserted.massY} should be ${b.y}")
    assert(inserted.total == 1, s"${inserted.total} should be 1")
  }

  test("Fork.insert(b) should return Fork with one body SE") {
    val quad = Fork(Empty(9f, 11f, 2f), Empty(11f, 11f, 2f), Empty(9f, 13f, 2f), Empty(11f, 13f, 2f))
    val b = new Body(3f, 11.5f, 13.5f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Fork(nw, ne, sw, se) =>
        assert(nw.total == 0, "NW of given Fork should be empty")
        assert(ne.total == 0, "NE of given Fork should be empty")
        assert(sw.total == 0, "SW of given Fork should be empty")
        assert(se.total == 1, "NW of given Fork should have one body")
      case _ =>
        fail("Fork.insert() should have returned a Fork, was $inserted")
    }
    assert(inserted.centerX == 10f, s"${inserted.centerX} should be 10f")
    assert(inserted.centerY == 12f, s"${inserted.centerY} should be 12f")
    assert(inserted.mass == b.mass, s"${inserted.mass} should be ${b.mass}")
    assert(inserted.massX == b.x, s"${inserted.massX} should be ${b.x}")
    assert(inserted.massY == b.y, s"${inserted.massY} should be ${b.y}")
    assert(inserted.total == 1, s"${inserted.total} should be 1")
  }

  test("Leaf.insert(b) should return Leaf if size is lower or equal than minimumSize (0.00001f)") {
    val initialBody = new Body(1f, 46f, 54f, 1f, 2f)
    val quad = Leaf(51f, 46.3f, 0.000001f, Seq(initialBody))
    val b = new Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 51f, s"$centerX should be 51f")
        assert(centerY == 46.3f, s"$centerY should be 46.3f")
        assert(size == 0.000001f, s"$size should be 5f")
        assert(bodies == Seq(b, initialBody), s"$bodies should contain the inserted body and initial body")
      case _ =>
        fail("Fork.insert() should have returned a Fork, was $inserted")
    }
  }

  test("Leaf.insert(b) should return Fork with one body in NW") {
    val quad = Leaf(10f, 12f, 4f, Seq())
    val b = new Body(1f, 9f, 11f, 1f, 2f)
    val inserted = quad.insert(b)
    inserted match {
      case Fork(nw, ne, sw, se) =>
        assert(nw.total == 1, "NW of given Fork should have one body")
        assert(ne.total == 0, "NE of given Fork should be empty")
        assert(sw.total == 0, "SW of given Fork should be empty")
        assert(se.total == 0, "SE of given Fork should be empty")
      case _ =>
        fail("Fork.insert() should have returned a Fork, was $inserted")
    }
    assert(inserted.centerX == 10f, s"${inserted.centerX} should be 10f")
    assert(inserted.centerY == 12f, s"${inserted.centerY} should be 12f")
    assert(inserted.mass == b.mass, s"${inserted.mass} should be ${b.mass}")
    assert(inserted.massX == b.x, s"${inserted.massX} should be ${b.x}")
    assert(inserted.massY == b.y, s"${inserted.massY} should be ${b.y}")
    assert(inserted.total == 1, s"${inserted.total} should be 1")
  }

  test("Leaf.insert(b) should return Fork with one body in NE") {
    val quad = Leaf(10f, 12f, 4f, Seq())
    val b = new Body(1f, 11f, 11f, 1f, 2f)
    val inserted = quad.insert(b)
    inserted match {
      case Fork(nw, ne, sw, se) =>
        assert(nw.total == 0, "NW of given Fork should be empty")
        assert(ne.total == 1, "NE of given Fork should have one body")
        assert(sw.total == 0, "SW of given Fork should be empty")
        assert(se.total == 0, "SE of given Fork should be empty")
      case _ =>
        fail("Fork.insert() should have returned a Fork, was $inserted")
    }
    assert(inserted.centerX == 10f, s"${inserted.centerX} should be 10f")
    assert(inserted.centerY == 12f, s"${inserted.centerY} should be 12f")
    assert(inserted.mass == b.mass, s"${inserted.mass} should be ${b.mass}")
    assert(inserted.massX == b.x, s"${inserted.massX} should be ${b.x}")
    assert(inserted.massY == b.y, s"${inserted.massY} should be ${b.y}")
    assert(inserted.total == 1, s"${inserted.total} should be 1")
  }

  test("Leaf.insert(b) should return Fork with one body in SW") {
    val quad = Leaf(10f, 12f, 4f, Seq())
    val b = new Body(1f, 9f, 13f, 1f, 2f)
    val inserted = quad.insert(b)
    inserted match {
      case Fork(nw, ne, sw, se) =>
        assert(nw.total == 0, "NW of given Fork should be empty")
        assert(ne.total == 0, "NE of given Fork should be empty")
        assert(sw.total == 1, "SW of given Fork should have one body")
        assert(se.total == 0, "SE of given Fork should be empty")
      case _ =>
        fail("Fork.insert() should have returned a Fork, was $inserted")
    }
    assert(inserted.centerX == 10f, s"${inserted.centerX} should be 10f")
    assert(inserted.centerY == 12f, s"${inserted.centerY} should be 12f")
    assert(inserted.mass == b.mass, s"${inserted.mass} should be ${b.mass}")
    assert(inserted.massX == b.x, s"${inserted.massX} should be ${b.x}")
    assert(inserted.massY == b.y, s"${inserted.massY} should be ${b.y}")
    assert(inserted.total == 1, s"${inserted.total} should be 1")
  }

  test("Leaf.insert(b) should return Fork with one body in SE") {
    val quad = Leaf(10f, 12f, 4f, Seq())
    val b = new Body(1f, 11f, 13f, 1f, 2f)
    val inserted = quad.insert(b)
    inserted match {
      case Fork(nw, ne, sw, se) =>
        assert(nw.total == 0, "NW of given Fork should be empty")
        assert(ne.total == 0, "NE of given Fork should be empty")
        assert(sw.total == 0, "SW of given Fork should be empty")
        assert(se.total == 1, "NW of given Fork should have one body")
      case _ =>
        fail("Fork.insert() should have returned a Fork, was $inserted")
    }
    assert(inserted.centerX == 10f, s"${inserted.centerX} should be 10f")
    assert(inserted.centerY == 12f, s"${inserted.centerY} should be 12f")
    assert(inserted.mass == b.mass, s"${inserted.mass} should be ${b.mass}")
    assert(inserted.massX == b.x, s"${inserted.massX} should be ${b.x}")
    assert(inserted.massY == b.y, s"${inserted.massY} should be ${b.y}")
    assert(inserted.total == 1, s"${inserted.total} should be 1")
  }

  // test cases for Body

  test("Body.updated should do nothing for Empty quad trees") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    assert(body.xSpeed == 0f)
    assert(body.ySpeed == 0f)
  }

  test("Body.updated should take bodies in a Leaf into account") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val body = b1.updated(quad)

    assert(body.xSpeed ~= 12.587037f)
    assert(body.ySpeed ~= 0.015557117f)
  }

  // test cases for sector matrix
  test("'SectorMatrix.normalizeBodyPosition' should always translate outbound body to boundaries area") {
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97

    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)

    assert((boundaries.minX, boundaries.minY) === sm.normalizeBodyPosition(new Body(3f, -1f, -1f, 0f, 0f)))
    assert((3f, boundaries.minY) === sm.normalizeBodyPosition(new Body(3f, 3f, -1f, 0f, 0f)))
    assert((5f, 7f) === sm.normalizeBodyPosition(new Body(3f, 5f, 7f, 0f, 0f)))
    assert((boundaries.maxX, boundaries.minY) === sm.normalizeBodyPosition(new Body(3f, 100f, -1f, 0f, 0f)))
    assert((boundaries.maxX, boundaries.maxY) === sm.normalizeBodyPosition(new Body(3f, 100f, 100f, 0f, 0f)))
  }

  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).exists(_ == body)
    assert(res, s"Body not found in the right sector")
  }

  test("'SectorMatrix.+=' should add a body to the correct bucket of a sector matrix") {
    val boundaries = new Boundaries()
    boundaries.minX = 0
    boundaries.minY = 0
    boundaries.maxX = 32
    boundaries.maxY = 32
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)

    var body = new Body(5, 1, 1, 0f, 0f)
    sm += body
    var res = sm(0, 0).size == 1 && sm(0, 0).exists(_ == body)
    assert(res, s"Body not found in the right sector")

    body = new Body(5, 5, 1, 0f, 0f)
    sm += body
    res = sm(1, 0).size == 1 && sm(1, 0).exists(_ == body)
    assert(res, s"Body not found in the right sector")

    body = new Body(5, 1, 5, 0f, 0f)
    sm += body
    res = sm(0, 1).size == 1 && sm(0, 1).exists(_ == body)
    assert(res, s"Body not found in the right sector")

    body = new Body(5, 31, 31, 0f, 0f)
    sm += body
    res = sm(7, 7).size == 1 && sm(7, 7).exists(_ == body)
    assert(res, s"Body not found in the right sector")
  }

  test("Simulator updateBoundaries should return proper min and max of x and y") {
    val simulator = new Simulator(defaultTaskSupport, new TimeStatistics)
    var boundaries = new Boundaries()
    boundaries.minX = 10
    boundaries.minY = 10
    boundaries.maxX = 12
    boundaries.maxY = 12

    boundaries = simulator.updateBoundaries(boundaries, new Body(5, 11, 11, 0f, 0f))
    assert((boundaries.minX, boundaries.minY, boundaries.maxX, boundaries.maxY) ===(10, 10, 12, 12))

    boundaries = simulator.updateBoundaries(boundaries, new Body(5, 9, 11, 0f, 0f))
    assert((boundaries.minX, boundaries.minY, boundaries.maxX, boundaries.maxY) ===(9, 10, 12, 12))

    boundaries = simulator.updateBoundaries(boundaries, new Body(5, 8, 8, 0f, 0f))
    assert((boundaries.minX, boundaries.minY, boundaries.maxX, boundaries.maxY) ===(8, 8, 12, 12))

    boundaries = simulator.updateBoundaries(boundaries, new Body(5, 11, 7, 0f, 0f))
    assert((boundaries.minX, boundaries.minY, boundaries.maxX, boundaries.maxY) ===(8, 7, 12, 12))

    boundaries = simulator.updateBoundaries(boundaries, new Body(5, 13, 11, 0f, 0f))
    assert((boundaries.minX, boundaries.minY, boundaries.maxX, boundaries.maxY) ===(8, 7, 13, 12))

    boundaries = simulator.updateBoundaries(boundaries, new Body(5, 11, 14, 0f, 0f))
    assert((boundaries.minX, boundaries.minY, boundaries.maxX, boundaries.maxY) ===(8, 7, 13, 14))

    boundaries = simulator.updateBoundaries(boundaries, new Body(5, 15, 16, 0f, 0f))
    assert((boundaries.minX, boundaries.minY, boundaries.maxX, boundaries.maxY) ===(8, 7, 15, 16))
  }

  test("Simulator mergeBoundaries should return proper min and max of x and y") {
    val simulator = new Simulator(defaultTaskSupport, new TimeStatistics)
    val a = new Boundaries()
    a.minX = 10
    a.minY = 10
    a.maxX = 12
    a.maxY = 12

    val b = new Boundaries()
    b.minX = 11
    b.minY = 11
    b.maxX = 13
    b.maxY = 13

    val c = new Boundaries()
    c.minX = 10.5f
    c.minY = 10.5f
    c.maxX = 11.5f
    c.maxY = 11.5f

    var result = new Boundaries()

    result = simulator.mergeBoundaries(a, c)
    assert((result.minX, result.minY, result.maxX, result.maxY) ===(10, 10, 12, 12))

    result = simulator.mergeBoundaries(b, c)
    assert((result.minX, result.minY, result.maxX, result.maxY) ===(10.5, 10.5, 13, 13))

    result = simulator.mergeBoundaries(a, b)
    assert((result.minX, result.minY, result.maxX, result.maxY) ===(10, 10, 13, 13))
  }

  test("Simulator computeSectorMatrix should compose proper matrix") {
    val simulator = new Simulator(defaultTaskSupport, new TimeStatistics)
    val boundaries = new Boundaries()
    boundaries.minX = 0
    boundaries.minY = 0
    boundaries.maxX = 32
    boundaries.maxY = 32
    var bodies: Seq[Body] = Seq()

    for(i <- 2 to boundaries.maxX.toInt by 4; j <- 2 to boundaries.maxY.toInt by 4) {
      bodies = bodies :+ new Body(1f, i, j, 0f, 0f)
    }

    val sectorMatrix = simulator.computeSectorMatrix(bodies, boundaries)
    assert(sectorMatrix.sectorSize === 4)
    assert(sectorMatrix.matrix.length === 64)
  }
}

object FloatOps {
  private val precisionThreshold = 1e-4

  /** Floating comparison: assert(float ~= 1.7f). */
  implicit class FloatOps(val self: Float) extends AnyVal {
    def ~=(that: Float): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Long floating comparison: assert(double ~= 1.7). */
  implicit class DoubleOps(val self: Double) extends AnyVal {
    def ~=(that: Double): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Floating sequences comparison: assert(floatSeq ~= Seq(0.5f, 1.7f). */
  implicit class FloatSequenceOps(val self: Seq[Float]) extends AnyVal {
    def ~=(that: Seq[Float]): Boolean =
      self.size == that.size &&
        self.zip(that).forall { case (a, b) =>
          abs(a - b) < precisionThreshold
        }
  }

}

