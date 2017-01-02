package org.coursera.scala.design.week4.observer

class BankAccount extends Publisher {
  private var balance: Int = 0

  def currentBalance = balance

  def deposit(amount: Int): Unit =
    if (amount > 0) {
      balance = balance + amount
      publish()
    }

  def withdraw(amount: Int): Int =
    if (0 < amount && amount <= balance) {
      balance = balance - amount
      publish()
      balance
    } else throw new Error("insufficient funds")
}
