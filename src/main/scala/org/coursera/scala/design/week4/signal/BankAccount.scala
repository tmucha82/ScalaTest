package org.coursera.scala.design.week4.signal

class BankAccount {
  val balance = Var(0)

  def deposit(amount: Int): Unit =
    if (amount > 0) {
      val currentBalance = balance()
      balance() = currentBalance + amount
    }

  def withdraw(amount: Int): Int =
    if (0 < amount && amount <= balance()) {
      val currentBalance = balance()
      balance() = currentBalance - amount
      balance()
    } else throw new Error("insufficient funds")
}
