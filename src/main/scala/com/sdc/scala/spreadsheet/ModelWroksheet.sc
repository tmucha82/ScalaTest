val height = 4
val width = 10

case class Cell(row: Int, column: Int)

val cells: Array[Array[Cell]] = (for {
  i <- 0 until height
  j <- 0 until width
} yield (i, j)).toArray.map { case (x, y) => Cell(x, y) }.grouped(width).toArray

cells.foreach {
  _.foreach(print)
}
