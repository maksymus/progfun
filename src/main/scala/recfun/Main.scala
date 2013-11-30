package recfun
import common._

object Main {
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
   */
  def pascal(c: Int, r: Int): Int =
    if (r <= c || c == 0) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def isOpen(char: Char): Boolean = char == '('

    def isClose(char: Char): Boolean = char == ')'

    def balance(chars: List[Char], cnt: Int): Boolean = {
      if (chars.isEmpty) cnt == 0
      else if (isOpen(chars.head)) balance(chars.tail, cnt + 1)
      else if (isClose(chars.head)) cnt != 0 && balance(chars.tail, cnt - 1)
      else balance(chars.tail, cnt)
    }

    if (chars.isEmpty) true
    else balance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
