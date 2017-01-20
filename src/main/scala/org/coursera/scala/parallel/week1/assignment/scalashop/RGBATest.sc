import org.coursera.scala.parallel.week1.assignment.scalashop._

val src = new Img(5, 5)
val dst = new Img(5, 5)
for (x <- 0 until 5; y <- 0 until 5) src(x, y) = rgba(x, y, x + y, math.abs(x - y))
rgbaToString(boxBlurKernel(src, 2, 2, 2))
rgbaToString(boxBlurKernel(src, 0, 0, 2))
rgbaToString(boxBlurKernel(src, 4, 4, 2))
VerticalBoxBlur.blur(src, dst, 0, 4, 2)
dst
val result1 = new Img(5, 5)
VerticalBoxBlur.parBlur(src, result1, 1, 2)
VerticalBoxBlur.parBlur(src, result1, 2, 2)
VerticalBoxBlur.parBlur(src, result1, 3, 2)
VerticalBoxBlur.parBlur(src, result1, 4, 2)
VerticalBoxBlur.parBlur(src, result1, 5, 2)
result1
val result2 = new Img(5, 5)
HorizontalBoxBlur.parBlur(src, result2, 1, 2)
HorizontalBoxBlur.parBlur(src, result2, 2, 2)
HorizontalBoxBlur.parBlur(src, result2, 3, 2)
HorizontalBoxBlur.parBlur(src, result2, 4, 2)
HorizontalBoxBlur.parBlur(src, result2, 5, 2)
result2
