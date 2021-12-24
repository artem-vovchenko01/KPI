package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("(yes)".toList))
    println(balance("(no(".toList))
    println(countChange(5, List(1, 2, 5)))
    println(pascal(1000,999))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def inner(tail: List[Char], nestLevel: Int): Boolean = {
      if (tail.isEmpty) {
        if (nestLevel == 0) true
        else false
      } else if (nestLevel < 0) false else {
        val head = tail.head
        if (head == '(') inner(tail.tail, nestLevel + 1)
        else if (head == ')') inner(tail.tail, nestLevel - 1)
        else inner(tail.tail, nestLevel)
      }
    }

    inner(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def branch(coins: List[Int], sum: Int): Int = {
      def total(coinsLeft: List[Int], sumCur: Int): Int = {
        if (coinsLeft.isEmpty) 0
        else branch(coinsLeft, sum + coinsLeft.head) + total(coinsLeft.tail, sumCur)
      }

      if (sum > money) 0
      else {
        if (sum == money) 1
        else total(coins, sum)
      }
    }

    branch(coins.sorted, 0)
  }

  /**
   * Function var 18
   */
  def var18(k: Int, x: Int): Int = {
    if (x > 1 && x < k) k
    else {
      require(x > k && x >= 0, "values are invalid")
      fact (x)
    }
  }

  def fact(x: Int): Int = {
    if (x <= 0) 1
    else x * fact(x - 1)
  }

}