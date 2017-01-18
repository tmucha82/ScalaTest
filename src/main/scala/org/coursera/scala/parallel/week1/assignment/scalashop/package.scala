package org.coursera.scala.parallel.week1.assignment

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  def rgba(rgba: Tuple4[RGBA, RGBA, RGBA, RGBA]): RGBA = {
    (rgba._1 << 24) | (rgba._2 << 16) | (rgba._3 << 8) | (rgba._4 << 0)
  }

  def rgbaToString(color: RGBA): String = Array(red(color), green(color), blue(color), alpha(color)).mkString("(", ",", ")")

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))

    def apply(x: Int, y: Int): RGBA = data(y * width + x)

    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c

    override def toString: String = {
      data.sliding(width, width).map(row => row.map(rgba => rgbaToString(rgba)).mkString("[", ",", "]")).mkString("\n", "\n", "")
    }
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    rgba(avgPixel(getSurroundingPixels(src, x, y, radius)))
  }

  /**
    * Get surrounding pixels (RGBA) of given pixel (x, y) of given image.
    *
    * @param src    input image
    * @param x      coordinate of x
    * @param y      coordinate of y
    * @param radius how big surrounding is
    * @return collection of RGBA which is represented by (Int, Int, Int, Int)
    */
  private def getSurroundingPixels(src: Img, x: Int, y: Int, radius: Int): List[(RGBA, RGBA, RGBA, RGBA)] = {
    (for {
      i <- -radius to radius
      j <- -radius to radius
    } yield (clamp(x + i, 0, src.width - 1), clamp(y + j, 0, src.height - 1))).distinct.map {
      case (a, b) =>
        val value = src(a, b)
        (red(value), green(value), blue(value), alpha(value))
    }.toList
  }

  /**
    * Calculates average value of list of pixels
    *
    * @param pixels collection of pixels to sum
    * @return result of summing
    */
  private def avgPixel(pixels: List[(RGBA, RGBA, RGBA, RGBA)]): (RGBA, RGBA, RGBA, RGBA) = {
    val sum = pixels.foldLeft((0, 0, 0, 0)) {
      case ((accRed, accGreen, accBlue, accAlpha), (red, green, blue, alpha)) => (accRed + red, accGreen + green, accBlue + blue, accAlpha + alpha)
    }
    (sum._1 / pixels.length, sum._2 / pixels.length, sum._3 / pixels.length, sum._4 / pixels.length)
  }
}
