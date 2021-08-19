package exercism.Group1

import scala.annotation.tailrec

object Raindrops {

  val lst: List[(Int, String)] = List((3, "Pling"), (5, "Plang"), (7, "Plong"))

  def convert4(num: Int): String = {
    val strOfSounds = lst.map(el => if(num%el._1 == 0) el._2 else "").mkString
    if(strOfSounds.nonEmpty) strOfSounds else num.toString
  }

    //The best solution
  def convert1(number: Int): String = {
    val res = lst.foldLeft("")((a, b) => if (number % b._1 == 0) a + b._2 else a)
    if (res.nonEmpty) res else number.toString
  }

  //Others
  //recursive
  def convert2(number: Int): String = {
    def helper(lst: List[(Int, String)]): String = {
      lst match {
        case h :: tail => if (number % h._1 == 0) h._2 + helper(tail) else helper(tail)
        case Nil => ""
      }
    }

    val res = helper(lst)
    if (res.nonEmpty) res else number.toString
  }

  //tail recursive
  def convert(number: Int): String = {
    @tailrec
    def helper(lst: List[(Int, String)], acc: String): String = {
      lst match {
        case h :: tail => if (number % h._1 == 0) helper(tail, acc + h._2) else helper(tail, acc)
        case Nil => acc
      }
    }

    val res = helper(lst, "")
    if (res.nonEmpty) res else number.toString
  }

}
