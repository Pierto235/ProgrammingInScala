package exercism.Group1

object DifferenceOfSquares {

  def squareOfSum(n: Int): Int =  Math.pow(n * (n + 1) / 2, 2).toInt

  def sumOfSquares(n: Int): Int = n * (n + 1) * (2*n + 1) / 6

  def differenceOfSquares(n: Int): Int = squareOfSum(n) - sumOfSquares(n)

}
