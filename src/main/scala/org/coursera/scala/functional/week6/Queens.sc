/**
  *
  * @param n number of rows
  * @return set of solututions
  */
def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] = {
    def isSafe(col: Int, queens: List[Int]) = {
      val row = queens.length
      val queensWithRow = (row - 1 to 0 by -1).zip(queens)
      queensWithRow forall {
        case (r, c) => col != c && math.abs(col - c) != row - r
      }
    }

    if (k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens
  }

  placeQueens(n)
}
def show(queens: List[Int]): String = {
  val lines =
    for {
      col <- queens.reverse
    } yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  "\n" + lines.mkString("\n")
}
queens(4)
(queens(4) map show) mkString "\n"
(queens(8) take 3 map show) mkString "\n"

