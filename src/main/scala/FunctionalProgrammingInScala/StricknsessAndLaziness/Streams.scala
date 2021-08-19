package FunctionalProgrammingInScala.StricknsessAndLaziness

object Streams extends App {

  /** For instanct, to find the second prime number between 1000 and 10000 */

  def isPrime(n: Int):Int = ???

 // ((1000 to 10000) filter isPrime)(1)

  // This is much shorter than recursive alternative, but is much less efficient

  /**
   * The answer to this is to avoid computing the tail of a sequence until it is
   * needed for the evaluation result (which might be never)

   * This idea is implemented in a new class, the Stream.

   * Stream is similar to lists, but their tail is evaluated only on demand

   */

  // How we produce Streams

  //1.
  val xs = Stream.cons(1, Stream.cons(2, Stream.empty))

  //2.
  val ys = Stream(1,2,3)

  //3.
  val zs = (1 to 3)toStream   //Stream[Int] = Stream(1,?)


  /**
   Lets produce Stream and List range by recursion
   */

  def streamRange(lo:Int, hi:Int): Stream[Int] =
    if(lo >= hi) Stream.empty
    else Stream.cons(lo, streamRange(lo+1, hi))   // +::

  //  x #:: xs <=> Stream(x, xs)
  //  x :: xs  <=> List(x, xs)

  def listRange(lo:Int, hi:Int): List[Int] =
    if(lo >= hi) Nil
    else lo :: listRange(lo+1, hi)


  // Stream supports almost all methods of List so we can write

  //((1000 to 10000).toStream filter isPrime)(1)


  /**
     * The implementation of streams that you've seen in the last session solves the
    problem of avoiding unnecessary computations when the tail value of the
    stream is not needed, but it suffers from another very serious potential performance
    problem. And that is that if tail is called several
    times the corresponding stream will be recomputed each time tail is called.

    Fortunately this problem can be avoided by storing the result of the first evaluation
    of tail and reusing the stored result instead of recomputing it the second and
    third times and all other times around.

    We can convince our self, that this
    optimization is sound, since the pure function of language and expression,
    produces the same result each time it is evaluated.

    So instead of re-evaluating the same expression several times.
    We could just squirrel away the first time we have produced the result, and reuse
    that result every time. That scheme is called lazy evaluation, as
    opposed to the call by name evaluation.

    Well, there's one, or maybe two problems
    with lazy evaluation which are essentially rooted in the fact that lazy evaluation is
    quite unpredictable in when computations happen.
    And how much space they take.

   */

  def expr = {

    val x = {println("x"); 1 }
    lazy val y = {println("y"); 2}
    def z = {println("z"); 3}

    z + y + x + z + y + x
  }

  expr

  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30?")
    i < 30
  }

  def greaterThan20(i: Int): Boolean = {
    println(s"$i is greater than 20?")
    i > 20
  }

  val numbers = List(1, 25, 40, 5, 23)

  val lt30 = numbers.filter(el => lessThan30(el))
  //List(1, 25, 5, 23)
  val gt20 = lt30.filter(el => greaterThan20(el))

  println(gt20)

  val lt30lazy = numbers.withFilter(el => lessThan30(el))
  val gt20lazy = lt30lazy.withFilter(el => greaterThan20(el))
  println
  //println(gt20lazy)
  gt20lazy.foreach(println)

  // for comprehensions use withFilter with guards
  for {
    a <- List(1,2,3) if a%2 == 0
  } yield a + 1

  List(1,2,3).withFilter(el => el %2 == 0).map(el => el + 1)



}
