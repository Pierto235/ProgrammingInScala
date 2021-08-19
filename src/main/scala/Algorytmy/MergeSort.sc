
import scala.util.Random
// merge sort algorithm by recursion

def merge(halfOne: List[Int], halfTwo: List[Int]): List[Int] = {
  (halfOne, halfTwo) match {
    case (Nil, halfT) => halfT
    case (halfO, Nil) => halfO
    case (a::tailOne, b::tailTwo) =>
      if (a<=b) a::merge(tailOne, halfTwo)
      else b::merge(halfOne, tailTwo)
  }
}

def mergeSort(lst: List[Int]): List[Int] = {
  if(lst.length < 2) lst
  else {
    val (halfOne, halfTwo) = lst.splitAt(lst.length / 2)
    merge(mergeSort(halfOne), mergeSort(halfTwo))
  }
}

val lst = List.fill(20)(Random.nextInt(100))
mergeSort(lst)