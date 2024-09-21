package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if  c < r then
      if c > 0 then pascal(c, r -1) + pascal(c-1, r-1) else 1
    else
      if c==r then 1 else 0

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    def balance_with_counter(chars: List[Char], counter: Int, open: Boolean): Boolean =
      if chars.isEmpty then
        counter == 0 && !open
      else
        if chars.head == '(' then
          balance_with_counter(chars.tail, counter+1, true)
        else
          if chars.head == ')' then
            balance_with_counter(chars.tail, counter-1, false)
          else
            balance_with_counter(chars.tail, counter, open)
    balance_with_counter(chars, 0, false)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    def sub_countChange(money: Int, coins: List[Int]): Int =
      if (money <= 0)
        if money == 0 then 1 else 0
      else if (coins.isEmpty)
          if money == 0 then 1 else 0
      else
          sub_countChange(money - coins.head, coins) + sub_countChange(money, coins.tail)
    sub_countChange(money, coins)


