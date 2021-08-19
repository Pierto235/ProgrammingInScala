import scala.util.Random

def quickSort(lst: List[Int]): List[Int] = {
  lst match {
    case Nil => Nil
    case a::Nil => List(a)
    case h::tail => {
      val pivot = h
      val (leading, trailing) = tail.partition(_ < pivot)
      quickSort(leading) ::: (pivot :: quickSort(trailing))
    }
  }
}

val lst = List.fill(10)(Random.nextInt(100))
quickSort(lst)