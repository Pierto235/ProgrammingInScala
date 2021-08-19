package FunctionalProgrammingInScala.Functors

object FunctorsExample extends App {

  trait Functor[F[_]] {
    def map[T, Y](l: F[T])(f: T => Y): F[Y]
  }


  val listFunctor = new Functor[List] {
    override def map[T, Y](l: List[T])(f: (T) => Y): List[Y] = l.map(f)
  }

    val numbers = List(1, 2, 3, 4, 5, 6)
    val mapping = Map(
      1 -> "one",
      2 -> "two",
      3 -> "three",
      4 -> "four",
      5 -> "five",
      6 -> "six"
    )

    println(s"The numbers doubled are:${listFunctor.map(numbers)(_ * 2)}")
    println(s"The numbers with strings are:${listFunctor.map(numbers)(i => (i, mapping(i)))}")


  val optFunctor = new Functor[Option] {
    override def map[T,Y](option: Option[T])(f: T => Y): Option[Y] =
      option match {
        case Some(value) => Some(f(value))
        case None => None
      }
  }

  val num = Some(2)

  println(s"Use of option as a functor: ${optFunctor.map(num)(a=>((a*a).toString))}")


}
