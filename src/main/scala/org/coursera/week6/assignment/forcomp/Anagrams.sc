import org.coursera.week6.assignment.forcomp.Anagrams._

def wordOccurrences(w: Word): Occurrences =
  w.groupBy(char => char.toLower).mapValues(_.length).toList.sorted

def sentenceOccurrences(s: Sentence) = wordOccurrences(s.mkString)

wordOccurrences("abcdAaCSDba")
sentenceOccurrences(List("Tomasz", "Mucha"))