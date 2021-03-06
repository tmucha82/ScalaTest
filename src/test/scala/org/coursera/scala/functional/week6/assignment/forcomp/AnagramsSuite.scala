package org.coursera.scala.functional.week6.assignment.forcomp

import org.coursera.scala.functional.week6.assignment.forcomp.Anagrams._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("check dictionary") {
    assert(Anagrams.dictionary.contains("zoo"))
    assert(Anagrams.dictionary.contains("Scala"))
    assert(Anagrams.dictionary.contains("cool"))
  }

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("wordOccurrences: abcdABSsc") {
    assert(wordOccurrences("abcdABSsc") === List(('a', 2), ('b', 2), ('c', 2), ('d', 1), ('s', 2)))
  }

  test("wordOccurrences: ScalaIsFun") {
    assert(wordOccurrences("ScalaIsFun") === List(('a', 2), ('c', 1), ('f', 1), ('i', 1), ('l', 1), ('n', 1), ('s', 2), ('u', 1)))
  }

  test("wordOccurrences: TomaszMucha") {
    assert(wordOccurrences("TomaszMucha") === List(('a', 2), ('c', 1), ('h', 1), ('m', 2), ('o', 1), ('s', 1), ('t', 1), ('u', 1), ('z', 1)))
  }

  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences: Tomasz Mucha") {
    assert(sentenceOccurrences("Tomasz" :: "Mucha" :: Nil) === List(('a', 2), ('c', 1), ('h', 1), ('m', 2), ('o', 1), ('s', 1), ('t', 1), ('u', 1), ('z', 1)))
  }

  test("sentenceOccurrences: ScalaIsFun") {
    assert(sentenceOccurrences("Scala" :: "Is" :: "Fun" :: Nil) === List(('a', 2), ('c', 1), ('f', 1), ('i', 1), ('l', 1), ('n', 1), ('s', 2), ('u', 1)))
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }

  test("dictionaryByOccurrences.get: Robert") {
    assert(dictionaryByOccurrences.get(List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1))).map(_.toSet) === Some(Set("Robert")))
  }

  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: aabbccc - abb") {
    val aabbccc = List(('a', 2), ('b', 2), ('c', 3))
    val abb = List(('a', 1), ('b', 2))
    val accc = List(('a',1), ('c',3))
    assert(subtract(aabbccc, abb) === accc)
  }

  test("subtract: aabbccc - empty list") {
    val aabbccc = List(('a', 2), ('b', 2), ('c', 3))
    val empty = List()
    assert(subtract(aabbccc, empty) === aabbccc)
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("combinations: cabcbac") {
    val cabcbac = List(('a', 2), ('b', 2), ('c', 3))
    val cabcbaccomb = List(
      List(),
      List(('c', 1)),
      List(('c', 2)),
      List(('c', 3)),
      List(('b', 1)),
      List(('b', 1), ('c', 1)),
      List(('b', 1), ('c', 2)),
      List(('b', 1), ('c', 3)),
      List(('b', 2)),
      List(('b', 2), ('c', 1)),
      List(('b', 2), ('c', 2)),
      List(('b', 2), ('c', 3)),
      List(('a', 1)),
      List(('a', 1), ('c', 1)),
      List(('a', 1), ('c', 2)),
      List(('a', 1), ('c', 3)),
      List(('a', 1), ('b', 1)),
      List(('a', 1), ('b', 1), ('c', 1)),
      List(('a', 1), ('b', 1), ('c', 2)),
      List(('a', 1), ('b', 1), ('c', 3)),
      List(('a', 1), ('b', 2)),
      List(('a', 1), ('b', 2), ('c', 1)),
      List(('a', 1), ('b', 2), ('c', 2)),
      List(('a', 1), ('b', 2), ('c', 3)),
      List(('a', 2)),
      List(('a', 2), ('c', 1)),
      List(('a', 2), ('c', 2)),
      List(('a', 2), ('c', 3)),
      List(('a', 2), ('b', 1)),
      List(('a', 2), ('b', 1), ('c', 1)),
      List(('a', 2), ('b', 1), ('c', 2)),
      List(('a', 2), ('b', 1), ('c', 3)),
      List(('a', 2), ('b', 2)),
      List(('a', 2), ('b', 2), ('c', 1)),
      List(('a', 2), ('b', 2), ('c', 2)),
      List(('a', 2), ('b', 2), ('c', 3))
    )
    assert(combinations(cabcbac).toSet === cabcbaccomb.toSet)
  }

  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }
}
