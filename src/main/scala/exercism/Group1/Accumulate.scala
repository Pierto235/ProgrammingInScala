package exercism.Group1

import scala.annotation.tailrec

class Accumulate {

  def accumulate1[A,B](f:A=>B, lst: List[A]): List[B] =
    lst match {
      case h::tail => f(h)::accumulate(f, tail)
      case Nil => Nil
    }

  def accumulate[A,B](f:A=>B, lst:List[A]):List[B] = {
    @tailrec
    def helper(lst:List[A], acc:List[B]): List[B]= {
      lst match {
        case h::tail => helper(tail, acc:::List(f(h)))
        case Nil => acc
      }
    }
    helper(lst, Nil)
  }

}
