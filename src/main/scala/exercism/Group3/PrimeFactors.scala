package exercism.Group3

import scala.annotation.tailrec

object PrimeFactors {

  def factors(num: Long): List[Long] = {
    @tailrec
    def helper(n: Long, d: Long, acc: List[Long]): List[Long] = {
      n match {
        case n if n == d => acc ::: List(d)
        case n if n%d == 0 => helper(n/d, d, acc ::: List(d))
        case n => helper(n, d+1, acc)
      }
    }
    if(num == 1) List() else helper(num, 2, Nil)
  }

}
