package com.sdc.scala.currency

// A first (faulty) design of the Currency class
abstract class Currency {
  val amount: Long
  def designation: String
  override def toString = amount + " " + designation
  def +(that: Currency): Currency = ???
  def *(x: Double): Currency = ???
}