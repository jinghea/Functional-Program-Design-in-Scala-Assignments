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

    c match {
      case 0 => 1
      case `r` => 1
      case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    val stack = new scala.collection.mutable.Stack[Char]

    def pushToStack(c: Char) = {

      stack.size match {

        case 0 => stack.push(c)
        case _ => {

          (stack.top == '(' && c == ')') match {
            case true => stack.pop()
            case false => stack.push(c)
          }
        }
      }
    }

    chars.foreach(e => {
      e match {
        case '(' | ')' => pushToStack(e);
        case _ =>
      }
    }
    )

    stack.size == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    var count = 0;

    def calc(left: Int, first: Int, leftCoins: List[Int]): Unit = {

      (0 to left / first).foreach(e => {

        val takeParts = e * first
        val newLeft = left - takeParts

        newLeft match {
          case 0 => count += 1
          case _ => {

            leftCoins.isEmpty match {
              case true =>
              case _ => calc(newLeft, leftCoins.head, leftCoins.tail)
            }
          }
        }
      })

    }

    calc(money, coins.head, coins.tail)

    count

  }
}
