package org.coursera.scala.design.week4.observer

class Consolidator(observed: List[BankAccount]) extends Subscriber {
  observed.foreach(_.subcribe(this))

  private var total: Int = _
  compute()

  def totalBalance = total

  def compute() = total = observed.map(_.currentBalance).sum

  override def handle(publisher: Publisher) = compute()
}
