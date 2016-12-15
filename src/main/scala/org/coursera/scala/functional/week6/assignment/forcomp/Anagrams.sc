import org.coursera.scala.functional.week6.assignment.forcomp.Anagrams._

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
  * Returns a list of all anagram sentences of the given sentence.
  *
  * An anagram of a sentence is formed by taking the occurrences of all the characters of
  * all the words in the sentence, and producing all possible combinations of words with those characters,
  * such that the words have to be from the dictionary.
  *
  * The number of words in the sentence and its anagrams does not have to correspond.
  * For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
  *
  * Also, two sentences with the same words but in a different order are considered two different anagrams.
  * For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
  * `List("I", "love", "you")`.
  *
  * Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
  *
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
def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
  def sentenceAnagrams(occurrences: Occurrences): List[Sentence] = {
    if (occurrences.isEmpty) List(Nil)
    else
      for {
        element <- combinations(occurrences) if dictionaryByOccurrences.keySet(element)
        word <- dictionaryByOccurrences(element)
        rest <- sentenceAnagrams(subtract(occurrences, element))
      } yield word :: rest
  }
  sentenceAnagrams(sentenceOccurrences(sentence))
}

wordOccurrences("abcdAaCSDba")
sentenceOccurrences(List("Tomasz", "Mucha"))
combinations(List(('a', 2), ('b', 2)))
combinations(List(('a', 2), ('b', 2), ('c', 3)))
makeWord(List(('a', 2), ('b', 2)))
makeWord(List(('a', 2), ('b', 2), ('c', 3)))
subtract(List(('a', 2), ('b', 2), ('c', 3)), List(('a', 1), ('b', 2)))
sentenceAnagrams(List("Yes", "man"))
