//1. data parallel - for loop
def initializeArray(xs: Array[Int])(v: Int): Unit = {
  for (i <- xs.indices.par) {
    xs(i) = v
  }
}

val array = Array(0, 0, 0, 0)
initializeArray(array)(3)
array



//2. data parallel - for loop - MandelbrotSet
val image = Array[Int]()

def color(i: Int): Int = {
  //transform iteration to color
  0
}

def coordinatesFor(idx: Int):(Double, Double) = {
  //coordinates for iteration
  (0d,0d)
}

val maxIterations = 100

def computePixel(xc: Double, yc: Double, maxIterations: Int): Int = {
  var i = 0
  var x, y = 0.0
  while (x * x + y * y < 4 && i < maxIterations) {
    val xt = x * x - y * y + xc
    val yt = 2 * x * y + yc
    x = xt
    y = yt
    i += 1
  }
  color(i)
}

def parRender(): Unit = {
  for (idx <- image.indices.par) {
    val (xc, yc) = coordinatesFor(idx)
    image(idx) = computePixel(xc, yc, maxIterations)
  }
}