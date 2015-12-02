package com.sdc.scala.stateful

import org.scalatest.FunSuite

class StatefulTest extends FunSuite {

  test("Getter and setter of stateful class") {
    class BankAccount {
      var balance: Int = 0 //create getter("balance") and setter("balance_=")

      def deposit(amount: Int) {
        require(amount > 0)
        balance += amount
      }

      def withdraw(amount: Int): Boolean =
        if (amount > balance) false
        else {
          balance -= amount
          true
        }
    }

    val bankAccount = new BankAccount()

    //use of setter
    bankAccount.balance_=(100)
    // use of getter
    assert(100 === bankAccount.balance)
  }

  test("Default initialization of var and Defining a getter and setter without an associated field") {
    class Thermometer {

      var celsius: Float = _
      //the same as default of Float
      var test: Boolean = _

      def fahrenheit = celsius * 9 / 5 + 32

      def fahrenheit_=(f: Float) {
        celsius = (f - 32) * 5 / 9
      }

      override def toString = fahrenheit + "F/" + celsius + "C"
    }

    val thermometer = new Thermometer()
    assert(0.0 === thermometer.celsius)
    assert(false === thermometer.test)
    assert(32.0 === thermometer.fahrenheit)
    thermometer.fahrenheit = 32
    assert(0.0 === thermometer.celsius)
  }

}
