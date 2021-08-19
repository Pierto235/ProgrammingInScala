package exercism.Group1

import scala.annotation.tailrec

object MatchingBrackets {

  def isPaired(text: String): Boolean = {

    val bracesMap = Map('[' -> ']', '('-> ')' , '{' ->'}')
    val revMap = bracesMap.map{ case (a, b) => (b, a) }

    @tailrec
    def helper(lst: List[Char], stack: List[Char], res: Boolean): Boolean = {
      lst match {
        case h :: tail if bracesMap.keySet.contains(h) => helper(tail, h :: stack, true)
        case h :: tail if revMap.keySet.contains(h) => if (stack.nonEmpty && revMap(h) == stack.head) helper(tail, stack.tail, true) else false
        case _ :: tail => helper(tail, stack, res)
        case Nil => stack.isEmpty
      }
    }
    helper(text.toList, Nil, true)
  }

}
