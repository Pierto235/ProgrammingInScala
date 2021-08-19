
class Person(val firstName: String, val lastName: String) extends Ordered[Person] {
  def compare(that: Person): Int = {
    val lastNameComparison =
      lastName.compareToIgnoreCase(that.lastName)
    if(lastNameComparison != 0)
      lastNameComparison
    else
      firstName.compareToIgnoreCase(that.firstName)
  }
  override def toString: String = firstName + " " + lastName
}

val robert = new Person("Robert", "Jones")
val sally = new Person("Sally", "Smith")
robert < sally


def orderedMergeSort[T <: Ordered[T]](lst: List[T]): List[T] = {
  def merge(halfOne: List[T], halfTwo: List[T]): List[T] = {
    (halfOne, halfTwo) match {
      case (Nil, halfT) => halfT
      case (halfO, Nil) => halfO
      case (a::tailOne, b::tailTwo) =>
        if (a<=b) a::merge(tailOne, halfTwo)
        else b::merge(halfOne, tailTwo)
    }
  }
  if(lst.length < 2) lst
  else {
    val (halfOne, halfTwo) = lst.splitAt(lst.length / 2)
    merge(orderedMergeSort(halfOne), orderedMergeSort(halfTwo))
  }
}

val people: List[Person] = List(
  new Person("Larry", "Wall"),
  new Person("Andras", "Hejnsberg"),
  new Person("Guido", "van Rasun"),
  new Person("Alan", "Kay"),
  new Person("Yukihiro", "Matsumoto")
)

val orderedList = orderedMergeSort(people)