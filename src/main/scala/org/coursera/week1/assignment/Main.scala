package org.coursera.week1.assignment

object
Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    *
    * @param c number of column of Pascal's Triangle, counting from 0
    * @param r number of row of Pascal's Triangle, counting from 0
    * @return the value of number at that spot in the Pascal's Triangle
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    *
    * @param chars string which is verified against the balancing of parentheses
    * @return `true` if the balancing of parentheses is met
    */
  def balance(chars: List[Char]): Boolean = {
    def parenthesesStockSize(chars: List[Char], initBalanceStockSize: Int): Int = {
      if (initBalanceStockSize < 0) -1
      else {
        if (chars.isEmpty) initBalanceStockSize
        else if (chars.head == '(') parenthesesStockSize(chars.tail, initBalanceStockSize + 1)
        else if (chars.head == ')') parenthesesStockSize(chars.tail, initBalanceStockSize - 1)
        else parenthesesStockSize(chars.tail, initBalanceStockSize)
      }
    }
    parenthesesStockSize(chars, 0) == 0
  }

  /**
    * Exercise 3
    *
    * @param money amount of money to be changed
    * @param coins list of available coins
    * @return amount of ways we could change given amount of money
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (coins == Nil || coins.isEmpty || money < 0) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
