import org.coursera.scala.parallel.week1.assignment.scalashop._

val src = new Img(5, 5)
val dst = new Img(5, 5)
for (x <- 0 until 5; y <- 0 until 5) src(x, y) = rgba(x, y, x + y, math.abs(x - y))
rgbaToString(boxBlurKernel(src, 2, 2, 2))
rgbaToString(boxBlurKernel(src, 0, 0, 2))
rgbaToString(boxBlurKernel(src, 4, 4, 2))
VerticalBoxBlur.blur(src, dst, 0, 4, 2)
dst
val result = new Img(5, 5)
VerticalBoxBlur.parBlur(src, result, 5, 2)
result
