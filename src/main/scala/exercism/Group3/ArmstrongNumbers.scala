package exercism.Group3

object ArmstrongNumbers {
  def isArmstrongNumber(num: Int): Boolean =
    num == num.toString.map(n => Math.pow(n.asDigit, num.toString.length)).sum
}
