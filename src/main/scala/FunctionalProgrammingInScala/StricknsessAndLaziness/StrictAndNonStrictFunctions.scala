package FunctionalProgrammingInScala.StricknsessAndLaziness



object StrictAndNonStrictFunctions extends App {

  // Strict function evaluate all its arguments
  // Non - Strict function may choose to not evaluate some of its arguments

  def square(x: Double):Double = x*x

  square(41+1)
//  square(sys.error("failure"))


  //improving the syntax
  def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
    if(cond) onTrue() else onFalse()

  def if3[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if(cond) onTrue else onFalse

  if2( true , () => println("a"), () => println("b") )
  val a = if3( false , sys.error("failure"), 3 )
  println(a)

////////////////////////////////////

  def maybeTwice(b: Boolean, i: => Int): Int =
    if(b) i + i else 0

  val x = maybeTwice(true, {println("hi"); 1 + 41 })
  println(x)


  def maybeTwice2(b: Boolean, i: => Int): Int =
    if(b) {
      lazy val j = i
      j+j
    }  else 0
  val x2 = maybeTwice2(true, {println("hi"); 1 + 41 })
  println(x2)

  /** Non-strict function take a parameter by name rather than by value */


  sealed trait Stream[+A]
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]:Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

//    def take(n: Int): Stream[A] = this match {
//      case Cons(h, t) if n> 1 => cons(h(), t().take(n-1))
//      case Cons(h, _) if n == 1 => cons(h(), empty)
//      case _ => empty
//    }

  }

//  def headOption[A]:Option[A] = this match {
//    case Empty => None
//    case Cons(h, t) => Some(h())
//  }









}
