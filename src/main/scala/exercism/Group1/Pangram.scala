package exercism.Group1

import scala.annotation.tailrec

object Pangrams {

  //the best solution
  def isPangram1(text: String): Boolean = {
    val clearedText = text.toLowerCase()
    @tailrec
    def checkLetters(lst: List[Char]): Boolean = {
      lst match {
        case h::tail => if(clearedText.contains(h)) checkLetters(tail) else false
        case Nil => true
      }
    }
    checkLetters('a' to 'z' toList)
  }

  def isPangram(text: String): Boolean = {
    val clearedText = text.toLowerCase()
    ('a' to 'z').forall(clearedText.contains(_))
  }

}
