package exercism.Group3

object CollatzConjecture {

  def steps1(num: Int): Option[Int] = {
    if (num <= 0) None
    else {
      def helper(n: Int, steps: Int): Option[Int] = {
        n match {
          case 1 => Some(steps)
          case m if m % 2 == 0 => helper(m / 2, steps + 1)
          case m => helper(m * 3 + 1, steps + 1)
        }
      }
      helper(num, 0)
    }
  }

  def steps(num: Int):Option[Int] = {
    if(num <= 0) None
    else {
      if(num == 1) Some(0)
      else if(num % 2 == 0) Some(1 + steps1(num/2).get) else Some(1 + steps1(num*3 + 1).get)
    }
  }


}