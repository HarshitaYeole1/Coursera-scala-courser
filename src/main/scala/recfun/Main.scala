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

  /* The numbers at the edge of the triangle are all 1,
      and each number inside the triangle is the sum of the two numbers above it.
      Write a function that computes the elements of Pascal’s triangle by means of a recursive process.

      Do this exercise by implementing the pascal function in Main.scala,
      which takes a column c and a row r,
      counting from 0 and returns the number at that spot in the triangle.
      For example, pascal(0,2)=1,pascal(1,2)=2 and pascal(1,3)=3.
   */

  def pascal(c: Int, r: Int): Int = {
      (r, c) match {
        case (_, 0) => 1
        case (r, c) if c == r => 1
        case (r, c) => pascal(c-1, r-1) + pascal(c, r-1)
        //for row4,col1 => row3,col0 + row3,col1
        // for row4,col2 => row3,col1 + row3,col2
        //for row4,col3 => row3,col2 + row3,col3
      }
    }


  /**
    * Exercise 2
    */

  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) throw new IllegalArgumentException("List is empty")

    def checkMatch(chars: List[Char], countOpen: Int, countClose: Int): Boolean = {
      chars match {
        case Nil if countOpen != 0 => countOpen == countClose
        case Nil => false
        case head :: tail if head.equals('(') => checkMatch(tail, countOpen + 1, countClose)
        case head :: tail if head.equals(')') && countClose < countOpen => checkMatch(tail, countOpen, countClose + 1)
        case x => checkMatch(x.tail, countOpen, countClose)
      }
    }

    checkMatch(chars, 0, 0)
  }

  /**
    * Exercise 3
    */

  /*
  Write a recursive function that
  counts how many different ways you can make change for an amount, given a list of coin denominations.
  For example,
  there are 3 ways to give change for 4 if you have coins with denomination 1 and 2: 1+1+1+1, 1+1+2, 2+2.

  Do this exercise by implementing the countChange function inMain.scala.
  This function takes an amount to change, and a list of unique denominations for the coins.

  Once again, you can make use of functions isEmpty, head and tail on the list of integers coins.

  Hint: Think of the degenerate cases.
  How many ways can you give change for 0 CHF(swiss money)?
  How many ways can you give change for >0 CHF, if you have no coins?



   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
