import org.coursera.week6.assignment.forcomp.Anagrams._

def wordOccurrences(w: Word): Occurrences =
  w.groupBy(char => char.toLower).mapValues(_.length).toList.sorted
def sentenceOccurrences(s: Sentence) = wordOccurrences(s.mkString)

def test(x: List[Occurrences], y: List[Occurrences]) = y ++ (for (i <- x; j <- y) yield i :: j)

def combinations(occurrences: Occurrences) = {
  def accumulate(rest: List[Occurrences]): List[Occurrences] = rest match {
    case List() => List[Occurrences](Nil)
    case x :: y =>
      val acc = accumulate(y)
      acc ++ (for (i <- x; j <- acc) yield i :: j)
  }
  accumulate(occurrences.map { case (char, size) => (for (i <- 1 to size) yield (char, i)).toList })
}

wordOccurrences("abcdAaCSDba")
sentenceOccurrences(List("Tomasz", "Mucha"))
combinations(List(('a', 2), ('b', 2))).size
combinations(List(('a', 2), ('b', 2), ('c', 3)))
