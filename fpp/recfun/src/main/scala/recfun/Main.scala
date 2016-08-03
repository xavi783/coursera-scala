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

    def factorialExplicit(x: Int, current: => Int): Int = x match {
      case 0 => 1
      case 1 => current
      case _ => factorialExplicit(x - 1, current * (x - 1))
    }

    def factorial(n: Int): Int = {
      if (n < 0)
        throw new IllegalArgumentException("only positive values")
      else
        factorialExplicit(n, n)
    }

    if (c > r + 1)
      throw new IllegalArgumentException(s"Maximum allowed column is: ${r + 1}")
    else
      factorial(r) / (factorial(c) * factorial(r - c))
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    val _ini = '('
    val _end = ')'

    def delta(ch: Char): Int = if (ch == _ini) 1 else if (ch == _end) -1 else 0

    def balanceExplicit(x: List[Char], current: Char, acc: Int): Boolean = {
      val _delta = delta(current)
      if (x.isEmpty)
        acc + _delta == 0
      else if (acc + _delta < 0)
        false
      else
        balanceExplicit(x.tail, x.head, acc + _delta)
    }

    if (chars.isEmpty)
      true
    else
      balanceExplicit(chars.tail, chars.head, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    val _coins = coins.sortWith(_ > _)

    def stopCondition(amount: Int, updatedSolution: Int): Int = {
      if (updatedSolution < amount) 1
      else if (updatedSolution == amount) 0
      else -1
    }

    def pickSolution(amount: Int, nodes: List[Int], invariant: Int, result: Int): Int = {
      if (nodes.isEmpty)
        result
      else
        println(s"testing: ${nodes.head}, current amount: $invariant, objective: $amount")
        stopCondition(amount, invariant + nodes.head) match {
          case 1 => pickSolution(amount, nodes, invariant + nodes.head, result)
          case 0 => pickSolution(amount, nodes.tail, invariant + nodes.head, result + 1)
          case _ => pickSolution(amount, nodes.tail, invariant, result)
        }
    }

    if (coins.isEmpty || money == 0)
      0
    else
      pickSolution(money, _coins, 0, 0)
  }
}
