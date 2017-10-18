package org.coursera.scala.spark.week2.assignment.stackoverflow

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Ignore, BeforeAndAfterAll, FunSuite}

@Ignore
@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

    override def langSpread = 50000

    override def kmeansKernels = 45

    override def kmeansEta: Double = 20.0D

    override def kmeansMaxIterations = 120

    val raw = rawPostings(StackOverflow.sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")).cache()
  }

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  test("groupedPostings for some data") {
    val postings = StackOverflow.sc.parallelize(List(
      Posting(1, 27233496, None, None, 0, Some("C#")),
      Posting(1, 23698767, None, None, 9, Some("C#")),
      Posting(1, 5484340, None, None, 0, Some("C#")),
      Posting(2, 5494879, None, Some(5484340), 1, None),
      Posting(1, 9419744, None, None, 2, Some("Objective-C")),
      Posting(1, 26875732, None, None, 1, Some("C#")),
      Posting(1, 9002525, None, None, 2, Some("C++")),
      Posting(2, 9003401, None, Some(9002525), 4, None),
      Posting(2, 9003942, None, Some(9002525), 1, None)
    ))
    val groupedPosting = testObject.groupedPostings(postings)
    val result = groupedPosting.collect()

    assert(2 === result.length)

    /**
      * Posting(1, 5484340, None, None, 0, Some("C#")) -> Posting(2, 5494879, None, Some(5484340), 1, None)
      */
    val questionOne = result.filter { case (qId, _) => qId == 5484340 }.head
    assert(questionOne._1 === 5484340)
    assert(questionOne._2.size === 1)
    assert(questionOne._2.head._1 === Posting(1, 5484340, None, None, 0, Some("C#")))
    assert(questionOne._2.head._2 === Posting(2, 5494879, None, Some(5484340), 1, None))

    /**
      * Posting(1, 9002525, None, None, 2, Some("C++")) -> [
      * Posting(2, 9003401, None, Some(9002525), 4, None),
      * Posting(2, 9003942, None, Some(9002525), 1, None),
      * Posting(2, 9005311, None, Some(9002525), 0, None)
      * ]
      */
    val questionTwo = result.filter { case (qId, _) => qId == 9002525 }.head
    assert(questionTwo._1 === 9002525)
    assert(questionTwo._2.size === 2)
    assert(questionTwo._2.head._1 === Posting(1, 9002525, None, None, 2, Some("C++")))
    assert(questionTwo._2.head._2 === Posting(2, 9003401, None, Some(9002525), 4, None))
    assert(questionTwo._2.tail.head._1 === Posting(1, 9002525, None, None, 2, Some("C++")))
    assert(questionTwo._2.tail.head._2 === Posting(2, 9003942, None, Some(9002525), 1, None))
  }

  ignore("groupedPostings for all data: count - should be 2121822") {
    val questionWithAnswersNumber = testObject.groupedPostings(testObject.raw).count()
    assert(2121822 === questionWithAnswersNumber)
  }

  ignore("groupedPostings for all data: take first element") {
    val questionWithAnswers = testObject.groupedPostings(testObject.raw).take(1).head
    assert(17743038 === questionWithAnswers._1)
  }

  test("scoredPostings for some data") {
    val groupedPosting = StackOverflow.sc.parallelize(List(
      (5484340, Seq(
        (Posting(1, 5484340, None, None, 0, Some("C#")), Posting(2, 5494879, None, Some(5484340), 1, None))
      ).toIterable),
      (9002525, Seq(
        (Posting(1, 9002525, None, None, 2, Some("C++")), Posting(2, 9003401, None, Some(9002525), 14, None)),
        (Posting(1, 9002525, None, None, 2, Some("C++")), Posting(2, 9003942, None, Some(9002525), 31, None))
      ).toIterable)
    ))

    val scoredPostings = testObject.scoredPostings(groupedPosting)
    val result = scoredPostings.collect()

    val firstScore = result.find { case (posting, _) => posting.id == 5484340 }.get
    assert(firstScore._1 === Posting(1, 5484340, None, None, 0, Some("C#")))
    assert(firstScore._2 === 1)

    val secondScore = result.find { case (posting, _) => posting.id == 9002525 }.get
    assert(secondScore._1 === Posting(1, 9002525, None, None, 2, Some("C++")))
    assert(secondScore._2 === 31)
  }

  ignore("scoredPostings for all data: count - should be 2121822") {
    val questionWithAnswers = testObject.groupedPostings(testObject.raw)
    val scoredPostings = testObject.scoredPostings(questionWithAnswers).count()
    assert(2121822 === scoredPostings)
  }

  ignore("scoredPostings for all data: take first element") {
    val questionWithAnswers = testObject.groupedPostings(testObject.raw)
    val scoredPosting = testObject.scoredPostings(questionWithAnswers).take(1).head
    assert(Posting(1, 17743038, None, None, 1, Some("Python")) === scoredPosting._1)
    assert(0 === scoredPosting._2)
  }

  test("vectorPostings for some data") {
    val scoredPosting = StackOverflow.sc.parallelize(List(
      (Posting(1, 6, None, None, 140, Some("CSS")), 67),
      (Posting(1, 42, None, None, 155, Some("PHP")), 89),
      (Posting(1, 72, None, None, 16, Some("Ruby")), 3),
      (Posting(1, 126, None, None, 33, Some("Java")), 30),
      (Posting(1, 174, None, None, 38, Some("C#")), 20),
      (Posting(1, 178, None, None, 38, Some("C#")), 34),
      (Posting(1, 179, None, None, 38, Some("Java")), 23),
      (Posting(1, 181, None, None, 38, Some("Ruby")), 7),
      (Posting(1, 182, None, None, 38, Some("COBOL")), 7)
    ))

    val vectorPostings = testObject.vectorPostings(scoredPosting)
    val result = vectorPostings.collect()
    assert((testObject.langs.indexOf("CSS") * testObject.langSpread, 67) === result(0))
    assert((testObject.langs.indexOf("PHP") * testObject.langSpread, 89) === result(1))
    assert((testObject.langs.indexOf("Ruby") * testObject.langSpread, 3) === result(2))
    assert((testObject.langs.indexOf("Java") * testObject.langSpread, 30) === result(3))
    assert((testObject.langs.indexOf("C#") * testObject.langSpread, 20) === result(4))
    assert((testObject.langs.indexOf("C#") * testObject.langSpread, 34) === result(5))
    assert((testObject.langs.indexOf("Java") * testObject.langSpread, 23) === result(6))
    assert((testObject.langs.indexOf("Ruby") * testObject.langSpread, 7) === result(7))
  }

  ignore("vectorPostings for all data: count - should be 2121822") {
    val questionWithAnswers = testObject.groupedPostings(testObject.raw)
    val scoredPostings = testObject.scoredPostings(questionWithAnswers)
    val vectorPostings = testObject.vectorPostings(scoredPostings).count()
    assert(2121822 === vectorPostings)
  }

  ignore("vectorPostings for all data: take first tree elements") {
    val questionWithAnswers = testObject.groupedPostings(testObject.raw)
    val scoredPostings = testObject.scoredPostings(questionWithAnswers)
    val result = scoredPostings.take(3)
    val vectorPostings = testObject.vectorPostings(scoredPostings)

    val test = vectorPostings.take(3)
    assert((testObject.langs.indexOf(result(0)._1.tags.get) * testObject.langSpread, result(0)._2) === test(0))
    assert((testObject.langs.indexOf(result(1)._1.tags.get) * testObject.langSpread, result(1)._2) === test(1))
    assert((testObject.langs.indexOf(result(2)._1.tags.get) * testObject.langSpread, result(2)._2) === test(2))
  }

  ignore("sampleVectors for all data") {
    val questionWithAnswers = testObject.groupedPostings(testObject.raw)
    val scoredPostings = testObject.scoredPostings(questionWithAnswers)
    val vectorPostings = testObject.vectorPostings(scoredPostings)
    val sampleVectors = testObject.sampleVectors(vectorPostings)
    val (lang, mean) = sampleVectors(0)
    assert(450000 === lang)
    assert(0 === mean)
  }

  test("clusterResults for some data") {
    val vectors = StackOverflow.sc.parallelize(List(
      (0, 2),
      (0, 3),
      (0, 4),
      (0, 1),
      (0, 3),
      (450000, 3),
      (450000, 2),
      (450000, 1),
      (450000, 3),
      (450000, 3),
      (450000, 3),
      (450000, 3),
      (450000, 3),
      (550000, 1130),
      (700000, 49),
      (700000, 49),
      (700000, 49),
      (700000, 49),
      (700000, 49),
      (700000, 49)
    ))
    val means = Array(
      (0, 3),
      (450000, 3),
      (450000, 3),
      (550000, 1130),
      (700000, 49),
      (700000, 49)

    )

    val result = testObject.clusterResults(means, vectors)
    assert(4 === result.length)
    assert(("JavaScript", 100.0, 5, 3) === result(0))
    assert(("Perl", 100.0, 8, 3) === result(1))
    assert(("Groovy", 100.0, 6, 49) === result(2))
    assert(("Haskell", 100.0, 1, 1130) === result(3))
  }
}
