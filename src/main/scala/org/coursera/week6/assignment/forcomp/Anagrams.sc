import org.coursera.week6.assignment.forcomp.Anagrams._

def wordOccurrences(w: Word): Occurrences =
  w.groupBy(char => char.toLower).mapValues(_.length).toList.sorted
def sentenceOccurrences(s: Sentence) = wordOccurrences(s.mkString)

def test(x: List[Occurrences], y: List[Occurrences]) = y ++ (for (i <- x; j <- y) yield i :: j)

def combinations(occurrences: Occurrences) = {
  def accumulate(rest: List[Occurrences]): List[Occurrences] = rest match {
    case List() => List(Nil)
    case occurrence :: restOccurrences =>
      val acc = accumulate(restOccurrences)
      acc ++ (for (charOcc <- occurrence; wordOcc <- acc) yield charOcc :: wordOcc)
  }
  accumulate(occurrences.map { case (char, size) => (for (i <- 1 to size) yield (char, i)).toList })
}

def makeWord(occurrences: Occurrences): Word = occurrences.map { case (char, frequency) => char.toString * frequency }.mkString
def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  val result = y.foldLeft(x)((previous, next) => {
    val (char, frequency) = next
    val map = previous.toMap.withDefaultValue(0)
    map.updated(char, map(char) - frequency).toList
  })
  result.filter { case (_, frequency) => frequency > 0 }.sorted
}

/**
  * List(en, as, my),
  * List(en, my, as),
  * List(man, yes),
  * List(men, say),
  * List(as, en, my),
  * List(as, my, en),
  * List(sane, my),
  * List(Sean, my),
  * List(my, en, as),
  * List(my, as, en),
  * List(my, sane),
  * List(my, Sean),
  * List(say, men),
  * List(yes, man)
  */
def sentenceAnagrams(sentence: Sentence) = {
  sentenceOccurrences(sentence)
  val occurrences = wordOccurrences(sentence.mkString)
  val allCombinations = combinations(occurrences)
  val dictionary = dictionaryByOccurrences.withDefaultValue(Nil)
  (for(combination <- allCombinations) yield dictionary(combination)).filterNot(_.isEmpty)
}

//in progress
sentenceAnagrams(List("Yes", "man"))
wordOccurrences("abcdAaCSDba")
sentenceOccurrences(List("Tomasz", "Mucha"))
combinations(List(('a', 2), ('b', 2)))
combinations(List(('a', 2), ('b', 2), ('c', 3)))
makeWord(List(('a', 2), ('b', 2)))
makeWord(List(('a', 2), ('b', 2), ('c', 3)))
subtract(List(('a', 2), ('b', 2), ('c', 3)), List(('a', 1), ('b', 2)))
