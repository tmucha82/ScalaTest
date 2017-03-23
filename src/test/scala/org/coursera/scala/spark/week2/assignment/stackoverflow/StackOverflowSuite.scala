package org.coursera.scala.spark.week2.assignment.stackoverflow

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}

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
        (Posting(1, 9002525, None, None, 2, Some("C++")), Posting(2, 9003401, None, Some(9002525), 4, None)),
        (Posting(1, 9002525, None, None, 2, Some("C++")), Posting(2, 9003942, None, Some(9002525), 1, None))
      ).toIterable)
    ))

    //TODO
    val scoredPostings = testObject.scoredPostings(groupedPosting)
    val result = scoredPostings.collect()
    result.foreach(println)
  }
}
