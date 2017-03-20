package org.coursera.scala.spark.week2

import org.apache.spark.{SparkConf, SparkContext}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ReductionOperationsTest extends FunSuite {

  test("fold operation") {
    lazy val sparkConfig = new SparkConf().setMaster("local").setAppName("ReductionOperations")
    lazy val sparkContext = new SparkContext(sparkConfig)

    case class Taco(kind: String, price: Double)

    val tacoOrders = List(
      Taco("A", 1.0),
      Taco("B", 2.0),
      Taco("C", 3.0),
      Taco("D", 4.0),
      Taco("R", 5.0),
      Taco("F", 6.0)
    )
    val tacoOrdersRdd = sparkContext.parallelize(tacoOrders)

    // calculate all order
    val sumOfTacos = tacoOrdersRdd.map(taco => taco.price).sum
    println(s"sumOfTacos = $sumOfTacos")

    sparkContext.stop()
  }

  test("agregate operation") {
    lazy val sparkConfig = new SparkConf().setMaster("local").setAppName("ReductionOperations")
    lazy val sparkContext = new SparkContext(sparkConfig)

    case class Taco(kind: String, price: Double)

    val tacoOrders = List(
      Taco("A", 1.0),
      Taco("B", 2.0),
      Taco("C", 3.0),
      Taco("D", 4.0),
      Taco("R", 5.0),
      Taco("F", 6.0)
    )
    val tacoOrdersRdd = sparkContext.parallelize(tacoOrders)

    // calculate all order
    val sumOfTacos = tacoOrdersRdd.aggregate(0.0)((price, taco) => price + taco.price, _ + _)
    println(s"sumOfTacos = $sumOfTacos")

    sparkContext.stop()
  }

}
