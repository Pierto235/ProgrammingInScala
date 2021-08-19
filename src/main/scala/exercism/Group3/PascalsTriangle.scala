package exercism.Group3

import scala.annotation.tailrec

object PascalsTriangle {

  def generateRow(lst: List[Int]): List[Int] = {
    @tailrec
    def helper(ls: List[Int], acc: List[Int]): List[Int] = {
      ls match {
        case List(1) => acc ::: List(1)
        case h::tail => helper(tail, acc:::List(h+tail.head))
      }
    }
    helper(lst, List(1))
  }


  def rows(row: Int): List[List[Int]] = {
    row match {
      case 1 => List(List(1))
      case r if r < 1 => List()
      case r => {
        val l = rows(r - 1)
        l ::: List(generateRow(l.last))
      }
    }
  }
}
