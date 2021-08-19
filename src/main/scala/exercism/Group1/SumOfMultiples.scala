package exercism.Group1

object SumOfMultiples {

  // Current best
  def sum(intSet: Set[Int], upperBound: Int): Int =
    intSet.flatMap(elem => elem until upperBound by elem).sum

  // Next
  def sum1(intSet: Set[Int], upperBound: Int): Int = {
    def findMultiples(start: Int, current: Int): List[Int] =
      if(current < upperBound) {
        current::findMultiples(start, start + current)
      } else Nil

    intSet.flatMap(el => findMultiples(el, el)).sum
  }

}
