class Poly(terms0: Map[Int, Double]) {
  val terms = terms0 withDefaultValue 0.0

  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  /*
    def +(other: Poly) = {
      def adjust(term: (Int, Double)): (Int, Double) = {
        val (x,y) = term
        terms get x match {
          case Some(yy) => (x, y + yy)
          case None => term
        }
      }
      new Poly(this.terms ++ (other.terms map adjust))
  */
  //old one - below using fold left
  def ++(other: Poly) = {
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (x, y) = term
      (x, terms(x) + y) // only withDefaultValue
    }
    new Poly(this.terms ++ (other.terms map adjust))
  }

  def +(other: Poly) = {
    new Poly(other.terms.foldLeft(this.terms)(addTerm))
  }

  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (x, y) = term
    terms + (x -> (terms(x) + y))
  }

  //nicer - but lets see how it woul be with or loop
  //  override def toString: String = ListMap(terms.toSeq.sorted.reverse: _*).transform { case (x, y) => y + " * x^" + x }.mkString(" + ")
  override def toString: String = {
    (for {
      (x, y) <- terms.toList.sorted.reverse
    } yield y + " * x^" + x).mkString(" + ")
  }
}

val p1 = new Poly(1 -> 2, 3 -> 4, 5 -> 6.2) //6.2*x^5 + 4*x^3 + 2*x
val p2 = new Poly(0 -> 3, 3 -> 7) // 7*x^3 + 3

p1 ++ p2
p1 + p2
p1.terms(7)
