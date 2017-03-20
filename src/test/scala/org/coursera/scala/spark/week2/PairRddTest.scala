package org.coursera.scala.spark.week2

import org.apache.spark.{SparkContext, SparkConf}
import org.coursera.scala.spark.week1.assignment.wikipedia.WikipediaArticle
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PairRddTest extends FunSuite {

  test("make pair rdd") {
    lazy val sparkConfig = new SparkConf().setMaster("local").setAppName("ReductionOperations")
    lazy val sparkContext = new SparkContext(sparkConfig)

    val wikiPages = List(
      WikipediaArticle("1", "blabla"),
      WikipediaArticle("2", "bleble"),
      WikipediaArticle("3", "blublu"),
      WikipediaArticle("4", "blibli")
    )
    val wikiPagesRdd = sparkContext.parallelize(wikiPages)

    // make pair Rdd
    val pairRdd = wikiPages.map(article => (article.title, article.text))
    pairRdd.foreach(println)

    sparkContext.stop()
  }
}
