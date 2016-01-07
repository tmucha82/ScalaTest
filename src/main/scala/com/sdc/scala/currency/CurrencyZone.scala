package com.sdc.scala.currency

// A first (faulty) design of the Currency class
abstract class CurrencyZone {
  type Currency <: AbstractCurrency
  val CurrencyUnit: Currency

  def make(amount: Long): Currency

  abstract class AbstractCurrency {
    val amount: Long

    def designation: String

    def +(that: Currency): Currency = make(this.amount + that.amount)

    def *(x: Double): Currency = make((this.amount * x).toLong)

    def -(that: Currency): Currency = make(this.amount - that.amount)

    def /(that: Double) = make((this.amount / that).toLong)

    def /(that: Currency) = this.amount.toDouble / that.amount

    //if you want other AbstractCurrency you need write CurrencyZone#AbstractCurrency.
    // if not it would be specific AbstractCurrency chosen
    def from(other: CurrencyZone#AbstractCurrency): Currency = make(Math.round(other.amount.toDouble * CurrencyConverter.exchangeRate(other.designation)(this.designation)))

    private def decimals(n: Long): Int = if (n == 1) 0 else 1 + decimals(n / 10)

    override def toString = (amount.toDouble / CurrencyUnit.amount.toDouble) formatted ("%." + decimals(CurrencyUnit.amount) + "f") + " " + designation
  }

}