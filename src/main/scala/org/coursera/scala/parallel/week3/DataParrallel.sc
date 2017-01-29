def initializeArray(xs: Array[Int])(v: Int): Unit = {
  for (i <- xs.indices.par) {
    xs(i) = v
  }
}

val array = Array(0, 0, 0, 0)
initializeArray(array)(3)
array