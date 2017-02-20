package recfun

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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == c || r <= 1)
      1
    else
      pascal(c-1, r-1) + pascal(c, r-1)

  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def balanceAux(chars: List[Char], openedParenthesis: Int): Boolean = {
      if (chars.isEmpty)
        if (openedParenthesis > 0 )
          false
        else
          true

      else if (chars.head == '(')
        balanceAux(chars.tail, openedParenthesis + 1)

      else if (chars.head == ')')
        if (openedParenthesis > 0)
          balanceAux(chars.tail, openedParenthesis - 1)
        else
          false

      else
        balanceAux(chars.tail, openedParenthesis)
    }

    balanceAux(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if(money > 0 && coins.nonEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else
      0

  }
}
