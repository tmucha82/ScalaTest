package com.sdc.scala.currency

import org.scalatest.FunSuite

class CurrencyZoneTest extends FunSuite {
  test("simple test of simple currency") {
    object TestCurrencyZone extends CurrencyZone {
      class TestCurrency extends AbstractCurrency {
        override def designation: String = "USD"
        override val amount: Long = 1000
      }
      override type Currency = TestCurrency
      override def make(x: Long): TestCurrencyZone.Currency = new TestCurrencyZone.TestCurrency {
        override val amount: Long = x
      }

      lazy override val CurrencyUnit: TestCurrencyZone.Currency = make(1)
    }
    println("1000 USD" === TestCurrencyZone.make(1000))
  }

  test("two currencies") {
    val dollars = US.make(1000)
    val euros = Europe.make(500)

    //dollars + euros //won't compile
    //euros + dollars //won't compile

    val moreDollarCash = dollars + dollars
    assert(US.make(2000).designation === moreDollarCash.designation) //OK!!
    assert(US.make(2000).amount === moreDollarCash.amount) //OK!!

    val moreEuroCash = euros + euros
    assert(Europe.make(1000).designation === moreEuroCash.designation) //OK!!
    assert(Europe.make(1000).amount === moreEuroCash.amount) //OK!!
  }

  test("from two currencies") {
    assert(Japan.make(12110).amount === (Japan.Yen from US.Dollar * 100).amount)
    assert(Japan.make(12110).designation === (Japan.Yen from US.Dollar * 100).designation)
  }
}
