package exercism.Group1

object FlattenArray {

  def flatten(lst: List[Any]): List[Int] =
    lst.flatMap {
      case null => Nil
      case i: Int => List(i)
      case ls: List[_] => flatten(ls)
    }

}
