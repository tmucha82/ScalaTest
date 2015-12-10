package com.sdc.scala.currency

import org.scalatest.FunSuite

class CurrencyTest extends FunSuite {
  test("simple test of simple currency") {
    val currency = new Currency {
      override def designation: String = "USD"
      override val amount: Long = 1000
    }
    println("1000 USD" === currency)
  }
}
