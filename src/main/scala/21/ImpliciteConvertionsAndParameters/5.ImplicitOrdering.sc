
def maxListOrdering[T](elements: List[T])(implicit ordering: Ordering[T]): T =
  elements match {
    case List() => throw new IllegalArgumentException("empty list")
    case List(x) => x
    case x:: rest =>
      val maxRest: T = maxListOrdering(rest)
      if(ordering.gt(x, maxRest)) x else maxRest

  }

maxListOrdering(List(1, 5, 10,3))
maxListOrdering(List(1.5, 5.2, 10.7, 3.14159))
maxListOrdering(List("one", "two", "three"))

case class Person(name: String, age: Int) extends Ordered[Person] {
  def compare(that: Person): Int = this.age - that.age
}

maxListOrdering(List(Person("Tom", 41),Person("Monika", 31), Person("Ada", 33)))

class MyInt(val i: Int)

implicit def IntToMyInt(o: Int) = new MyInt(o)
implicit def myintToDigits(i: MyInt) = i.i.toString.toList.map(_.asDigit)
implicit def myintToRange(i: MyInt) = 1 to i.i

val a: MyInt = 12