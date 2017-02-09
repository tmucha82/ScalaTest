package org.coursera.scala.parallel.week4.assignment

import org.coursera.scala.parallel.common._
import barneshut.conctrees._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(body: Body): Quad

    def contains(body: Body): Boolean = {
      val halfOfSize = size / 2
      val isBetween = (value: Float, center: Float) => value >= center - halfOfSize && value <= center + halfOfSize
      isBetween(body.x, centerX) && isBetween(body.y, centerY)
    }
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX

    def massY: Float = centerY

    def mass: Float = 0

    def total: Int = 0

    def insert(body: Body): Quad = Leaf(centerX, centerY, size, Seq(body))
  }

  case class Fork(nw: Quad, ne: Quad, sw: Quad, se: Quad) extends Quad {
    val quads = List(nw, ne, sw, se)

    val centerX: Float = nw.centerX + nw.size / 2
    val centerY: Float = nw.centerY + nw.size / 2
    val size: Float = nw.size + ne.size
    val mass: Float = quads.map(_.mass).sum
    val massX: Float = quads.map(quad => quad.massX * quad.mass).sum / mass
    val massY: Float = quads.map(quad => quad.massY * quad.mass).sum / mass
    val total: Int = quads.map(_.total).sum

    def insert(body: Body): Fork = {
      quads.map(quad => if (quad.contains(body)) quad.insert(body) else quad) match {
        case List(northWest, northEast, southWest, southEast) => Fork(northWest, northEast, southWest, southEast)
      }
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body]) extends Quad {
    val mass = bodies.map(_.mass).sum
    val massX = bodies.map(body => body.x * body.mass).sum / mass
    val massY = bodies.map(body => body.y * body.mass).sum / mass

    val total: Int = bodies.size

    def insert(body: Body): Quad = {
      if (size > minimumSize) {
        val quarterOfSize = size / 4
        val halfOfSize = size / 2
        val initQuad = Fork(
          Empty(centerX - quarterOfSize, centerY - quarterOfSize, halfOfSize),
          Empty(centerX + quarterOfSize, centerY - quarterOfSize, halfOfSize),
          Empty(centerX - quarterOfSize, centerY + quarterOfSize, halfOfSize),
          Empty(centerX + quarterOfSize, centerY + quarterOfSize, halfOfSize)
        )
        (body +: bodies).foldLeft(initQuad)(_.insert(_))
      } else {
        Leaf(centerX, centerY, size, body +: bodies)
      }
    }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xSpeed: Float, val ySpeed: Float) {

    def updated(quad: Quad): Body = {
      var netForceX = 0.0f
      var netForceY = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximaties get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximaties.
         */
        if (dist > 1f) {
          val dForce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dForceX = dForce * xn
          val dForceY = dForce * yn
          netForceX += dForceX
          netForceY += dForceY
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
        // no force
        case Leaf(_, _, _, bodies) => bodies.foreach(body => addForce(body.mass, body.x, body.y))
        // add force contribution of each body by calling addForce
        case fork@Fork(nw, ne, sw, se) =>
          if (quad.size / distance(quad.massX, quad.massY, x, y) < theta)
            addForce(quad.mass, quad.massX, quad.massY)
          else fork.quads.foreach(traverse)
        // see if node is far enough from the body,
        // or recursion is needed
      }

      traverse(quad)

      val nx = x + xSpeed * delta
      val ny = y + ySpeed * delta
      val nXSpeed = xSpeed + netForceX / mass * delta
      val nYSpeed = ySpeed + netForceY / mass * delta

      new Body(mass, nx, ny, nXSpeed, nYSpeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- matrix.indices) matrix(i) = new ConcBuffer

    def normalizeBodyPosition(b: Body): (Float, Float) = {
      val insideBoundaries = (value: Float) => math.min(boundaries.maxX, math.max(boundaries.minX, value))
      (insideBoundaries(b.x), insideBoundaries(b.y))
    }

    def +=(b: Body): SectorMatrix = {
      val (bodyX, bodyY) = normalizeBodyPosition(b)
      val x = ((bodyX - boundaries.minX) / sectorSize).toInt
      val y = ((bodyY - boundaries.minY) / sectorSize).toInt
      this (x, y) = b
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def update(x: Int, y: Int, b: Body): Unit = matrix(y * sectorPrecision + x) += b

    def combine(that: SectorMatrix): SectorMatrix = {
      val newMatrix = new SectorMatrix(boundaries, sectorPrecision)
      for (i <- matrix.indices)
        newMatrix.matrix(i) = matrix(i) combine that.matrix(i)
      newMatrix
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          val emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this (x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nSpan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nSpan, nAchievedParallelism),
              quad(x + nSpan, y, nSpan, nAchievedParallelism),
              quad(x, y + nSpan, nSpan, nAchievedParallelism),
              quad(x + nSpan, y + nSpan, nSpan, nAchievedParallelism)
            )
            else (
              quad(x, y, nSpan, nAchievedParallelism),
              quad(x + nSpan, y, nSpan, nAchievedParallelism),
              quad(x, y + nSpan, nSpan, nAchievedParallelism),
              quad(x + nSpan, y + nSpan, nSpan, nAchievedParallelism)
              )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: => T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        System.currentTimeMillis() - startTime
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: $totalTime ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString "\n"
    }
  }

}
