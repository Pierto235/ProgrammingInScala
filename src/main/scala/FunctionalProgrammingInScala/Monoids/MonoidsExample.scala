package FunctionalProgrammingInScala.Monoids

object MonoidsExample extends App {


  trait Monoid[T] {
    def op(l: T, r: T): T
    def zero: T
  }


  class ListMonoid extends Monoid[List[Int]] {
    val zero = Nil

    override def op(l: List[Int], r: List[Int]): List[Int] = l ::: r
  }
  val listIntMonoid = new ListMonoid
  listIntMonoid.op(Nil, List(1,2,3))


  val intAddition: Monoid[Int] = new Monoid[Int] {
    val zero: Int = 0

    override def op(l: Int, r: Int): Int = l + r
  }


  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    val zero: Int = 1

    override def op(l: Int, r: Int): Int = l * r
  }


  val stringConcatenation: Monoid[String] = new Monoid[String] {
    val zero: String = ""

    override def op(l: String, r: String): String = l + r
  }

  // Monoids are very useful with collection i.e. by applying foldLeft/Right
  def foldLeft[A](z: A)(f: (A, A) => A): A = ???
  def foldRight[A](z: A)(f: (A, A) => A): A = ???

  //Int is monoid with 0
  List(1,2,3).foldLeft(0)((a,b) => a+b)



  val strings = List("This is\n", "a list of\n", "strings!")
  val numbers = List(1, 2, 3, 4, 5, 6)
  println(s"Left folded:\n${strings.foldLeft(stringConcatenation.zero)(stringConcatenation.op)}")
  println(s"Right folded:\n${strings.foldRight(stringConcatenation.zero)(stringConcatenation.op)}")
  println(s"6! is: ${numbers.foldLeft(intMultiplication.zero)(intMultiplication.op)}")



  def fold1[T](list: List[T], m: Monoid[T]): T = list.foldLeft(m.zero)(m.op)

  fold1(numbers, intMultiplication)

//////////////////////////////////////////////////////////////////////////////////
  def fold[T](list: List[T], m: Monoid[T]): T = foldMap(list, m)(identity)

  // list = List(1,2,3): List[Int] , m = stringConcatenation: Monoid[String],  f(a)=a.toString
   def foldMap[T, Y](list: List[T], m: Monoid[Y])(f: T => Y): Y =
    list.foldLeft(m.zero) {
      case (a, y) => m.op(a, f(y))
    }

  println(foldMap(List(1,2,3), stringConcatenation)(a=>a.toString))


//////////////////////////////////////////////////////////////////////////////
  def balancedFold[T, Y](list: IndexedSeq[T], m: Monoid[Y])(f: T => Y): Y =
    if (list.length == 0) {
      m.zero
    } else if (list.length == 1) {
      f(list(0))
    } else {
      val (left, right) = list.splitAt(list.length / 2)
      m.op(balancedFold(left, m)(f), balancedFold(right, m)(f))
    }

  val arr_numbers = Array(1, 2, 3, 4)
  println(s"4! is: ${balancedFold(arr_numbers, intMultiplication)(identity)}")


///////////////////////////////////////////////////////////////////
  def foldPar[T](list: List[T], m: Monoid[T]): T =
    foldMapPar(list, m)(identity)

  def foldMapPar[T, Y](list: List[T], m: Monoid[Y])(f: T => Y): Y =
    list.par.foldLeft(m.zero) {
      case (t, y) => m.op(t, f(y))
    }


  println(s"Left folded:\n${foldPar(strings, stringConcatenation)}")
  println(s"Right folded:\n${foldPar(strings, stringConcatenation)}")
  println(s"6! is: ${foldPar(numbers, intMultiplication)}")

//////////////////////////////////////////////////////////////////////////

  def compose[T, Y](a: Monoid[T], b: Monoid[Y]): Monoid[(T, Y)] =
    new Monoid[(T, Y)] {
      val zero: (T, Y) = (a.zero, b.zero)

      override def op(l: (T, Y), r: (T, Y)): (T, Y) =
        (a.op(l._1, r._1), b.op(l._2, r._2))
    }


  val sumAndProduct = compose(intAddition, intMultiplication)
  println(s"The sum and product is: ${balancedFold(arr_numbers, sumAndProduct)(i => (i, i))}")


  ////////////////////////////////////////////////////////////////////////
  def mapMerge[K, V](a: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      override def zero: Map[K, V] = Map()

      override def op(l: Map[K, V], r: Map[K, V]): Map[K, V] =
        (l.keySet ++ r.keySet).foldLeft(zero) {
          case (res, key) => res.updated(key, a.op(l.getOrElse(key, a.zero), r.getOrElse(key, a.zero)))
        }
    }

  val features = Array("hello", "features", "for", "ml", "hello",
    "for", "features")
  val counterMonoid: Monoid[Map[String, Int]] = mapMerge(intAddition)
  println(s"The features are: ${balancedFold(features, counterMonoid)(i => Map(i -> 1))}")





}
