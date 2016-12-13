import org.coursera.week6.assignment.forcomp.Anagrams._

def wordOccurrences(w: Word): Occurrences =
//  w.toLowerCase.groupBy(char => (char, w.toLowerCase.count(c => char == c))).keys.toList.sorted
  w.groupBy(char => char.toLower).mapValues(_.length).toList.sorted


wordOccurrences("abcdAaCSDba")
