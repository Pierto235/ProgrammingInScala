//B<:A => List[B] <: List[A]
sealed trait List[+A]
case class Cons[+A](h: A, tail: List[A]) extends List[A]
case object Nil extends List[Nothing] //=>        (wszystko jest podtypem Nothing)?  <: Nothing =>  Nil <: List[Nothing]
                                                          // Int <: Nothing => List(): List[Int] <: List[Nothing]
                                                          //czyli Nil może sluc jako pusta list Int, Double, String, ...

object List {
  def sum(ds: List[Int]): Int = {
    ds match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def product(ds: List[Double]): Double = {
    ds match {
      case Nil => 1
      case Cons(x, xs) => x * product(xs)
    }
  }

  def apply[A](xs: A* ): List[A] = {  //variadic function (meaning : accepts zero or more arguments), which is syntactic sugar for Seq[A],
    if(xs.isEmpty) Nil else Cons(xs.head, apply(xs.tail: _*)) // _* allows us to pass a Seq to variadic method
  }

  def tail[A](xs: List[A]): List[A] = {
    xs match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, t) => t
    }
  }

  def setHead[A](xs: List[A], x: A): List[A] = {
    xs match {
      case Nil => sys.error("head of empty list")
      case Cons(_, t) => Cons(x, t)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) if n <= 0 => Cons(h, t)
      case Cons(_, t) => drop(t, n-1)
    }
  }

  def dropWhile[A](l: List[A], p:A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if p(h) => dropWhile(t, p)
      case _ => l
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  def last[A](ls: List[A]): List[A] = {
    ls match {
      case Nil => sys.error("empty list")
      case Cons(h, Nil) => Cons(h, Nil)
      case Cons(_, t) => last(t)
    }
  }

  def init[A](ls: List[A]): List[A] = {
    ls match {
      case Nil => sys.error("empty list")
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  // type inference
  def dropWhileImproved[A](l: List[A])( p:A => Boolean): List[A] = {
    l match {                         //: (A=>Boolean) => List[A]
      case Cons(h, t) if p(h) => dropWhile(t, p)
      case _ => l                     // (List[A], A=>Boolean) => List[A]
    }                                 // (List[A])(A=>Boolean) => List[A]
  }

  // generalizing sum and product - instead sum and product we create HOF
  // foldRight which can do the same thing
  // Right since B is on the right position in f, we execute recursively (not stack safe)
  // foldRight and at the end comes folding from the right side like (a + (b + (c +(d+z))))

  def foldRight[A, B](xs: List[A], z: B) (f:(A, B) => B): B = {
    xs match {
      case Nil => z
      case Cons(h, t) => f( h, foldRight(t, z)(f))
    }
  }

  val a = foldRight(List(1,2,3), 0)((a,b)=> a+b)
  val b = foldRight(List(1,2,3), Nil: List[Int])((a,xb)=> Cons(a,xb))
  val l = foldRight(List(1,2,3,4,5,6), 0)((a,b) => 1 + b )

  def foldLeft[A,B](xs: List[A], z: B)(f: (B,A)=>B): B = {
    xs match {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z,h))(f)
    }
  }
  val lst = List(1,2,3,4,5,6)
  val sfl = foldLeft(lst, 0)((x,y) => x+y)
  val pfl = foldLeft(lst, 1)((x,y) => x*y)
  val lfl = foldLeft(lst, 0)((x,y) => x+1)

  def reverse[A](xs: List[A]): List[A] =
    foldLeft(xs, Nil: List[A])((x,y)=>Cons(y,x))

  val rev = reverse(lst)

  def appendFR[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x,y) => Cons(x, y))

  def appendFL[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)((x,y) => Cons(y, x))

  val app = appendFR(List(1,2), List(3,4))
  val appFL = appendFL(List(1,2), List(3,4))


//  def foldLeftR[A,B](xs: List[A], z: B)(f:(B,A)=>B): B =
//    foldRight(xs, z)

  def transform(xs: List[Int]): List[Int] =
    xs match {
      case Nil => Nil
      case Cons(h,t) => Cons(h + 1, transform(t))
    }

  def transformF(xs:List[Int]): List[Int] =
    foldRight(xs, Nil: List[Int])((a,b) => Cons(a+1, b))

  def transformString(xs: List[Double]): List[String] =
    foldRight(xs, Nil: List[String])((a,b) => Cons(a.toString, b))

  def mapFR[A,B](xs: List[A])(f: A=>B): List[B] =
    foldRight(xs, Nil: List[B])((a,b) => Cons(f(a), b))

  def mapFL[A,B](xs: List[A])(f: A=>B): List[B] =
    foldLeft(xs, Nil: List[B])((a,b) => Cons(f(b), a))

  def filter[A](xs: List[A])(p:A => Boolean): List[A] =
    foldRight(xs, Nil: List[A])((a,b) => if(p(a)) Cons(a,b) else b )

  def flatMap[A,B](xs: List[A])(f: A => List[B]): List[B] =
    foldRight(xs, Nil: List[B])((a,b) => append(f(a), b) )

  def filterFM[A](xs: List[A])(p: A => Boolean): List[A] =
    flatMap(xs)(a => if(p(a)) List(a) else Nil)

  def construct(xs: List[Int], ys: List[Int]): List[Int] =
    (xs, ys) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, construct(t1,t2))
    }

  def zipWith[A,B,C](xs: List[A], ys: List[B])(f: (A,B) => C): List[C] =
    (xs, ys) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1,t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }


}

val le: List[Int] = Nil
val l: List[Int] = List(1,2,3)
val la = List.apply(1,2,3)
val l2 = List(4,5,6)
List.tail(List(1,2,3))
List.setHead(l, 0)
List.drop(l, 2)
List.drop(le, 2)
List.drop(l, 5)
List.dropWhile(l, (a: Int) => a<2 )
List.dropWhileImproved(l)(_<2 )
val ls = List.append(l, l2)
List.init(ls)
List.a
List.b
List.l
List.sfl
List.pfl
List.lfl
List.rev
List.app
List.appFL
val res = List.filter(l2)(a => a%2 == 0)
val res1 = List.flatMap(l2)(a=>List(a,a))
val res3 = List.construct(List(1,2,3), List(1,2,3))

case class Department(name: String, department: String)

