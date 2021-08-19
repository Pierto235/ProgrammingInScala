package FunctionalProgrammingInScala.GettingStarted

import scala.annotation.tailrec

object MyModule { // Module
  def abs(n: Int): Int =
    if(n>=0) n else -n


  def factorial(num: Int): Int = {
    @tailrec
    def helper(n: Int, acc: Int): Int = {
      if( n <= 0 ) acc
      else helper(n-1, acc * n)
    }
    helper(num, 1)
  }

  private def formatAbs(x: Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
    // s"The absolute value of $x is ${abs(x)}"
  }

  private def formatFactorial(x: Int): String = {
    val msg = "The factorial of %d is %d"
    msg.format(x, factorial(x))
  }

  private def formatResult(name: String, x: Int, f: Int=>Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(name, x, f.apply(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))

    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
  }

}

