// Singly linked list

// covariance: B <= A => List[B] <= List[A]
// sealed : all implementation of the trait must be in this file
sealed trait Lista[+A]
case object Nil extends Lista[Nothing]
case class Cons[+A](head: A, tail: Lista[A]) extends Lista[A]

object Lista {

  def sum(ints: Lista[Int]): Int =
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

  def product(ds: Lista[Double]): Double = ds match {
    case Nil => 1
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): Lista[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](xs: Lista[A]): Lista[A] =
    xs match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, xs) => xs
    }

  def setHead[A](x: A, xs: Lista[A]): Lista[A] =
    xs match {
      case Nil => sys.error("head of empty list")
      case Cons(_, xs) => Cons(x, xs)
    }

  def drop[A](n: Int, xs: Lista[A]): Lista[A] =
    if(n==0) xs
    else drop(n-1, tail(xs))

  def dropWhile[A](xs: Lista[A], p: A => Boolean): Lista[A] =
    xs match {
      case Nil => xs
      case Cons(x, xs) => if (p(x)) dropWhile(xs, p) else Cons(x, xs)
    }

  //efficiency of data sharing
  def append[A](a1: Lista[A], a2: Lista[A]): Lista[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: Lista[A]): Lista[A] = {
      l match {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
      }
  }

  // improving type inference for higher-order functions

  def dropWhile1[A](as: Lista[A])(f: A => Boolean): Lista[A] =
    as match {
      case Cons(h,t) if f(h) => dropWhile1(t)(f)
      case _ => as
    }

  // Recursion over lists (foldRight over List of Int for addition and multiplication)

  def foldRight[A,B](xs: Lista[A], n: B)(f: (A,B) => B): B = {
    xs match {
      case Nil => n
      case Cons(h, t) => f(h, foldRight(t, n)(f))
    }
  }

  def length[A](as: Lista[A]): Int =
    as match {
      case Nil => 0
      case Cons(h, t) => foldRight(Lista(1), 0)(_+_) + length(t)
    }

  def length2[A](as: Lista[A]): Int =
    as match {
      case Nil => 0
      case Cons(_, t) => foldRight(t, 1)((_,b) => 1 + b )
    }

  def foldLeft[A,B](as: Lista[A], z: B)(f: (B, A)=> B): B = {
    def helper(ls: Lista[A], acc: B): B = {
      ls match {
        case Nil => acc
        case Cons(h, t) => helper(t, f(acc, h))
      }
    }
    helper(as, z)
  }

  def sumFL(xs: Lista[Int]): Int =
    xs match {
      case Nil => 0
      case Cons(h,t) => foldLeft(t, h)(_+_)
    }

  def productFL(xs: Lista[Int]): Int =
    xs match {
      case Nil => 0
      case Cons(h,t) => foldLeft(t, h)(_*_)
    }

  def lengthFL(xs: Lista[Int]): Int =
    xs match {
      case Nil => 0
      case Cons(h,t) => foldLeft(t, 1)((a,b) => a + 1 )
    }

  def reverse[A](xs: Lista[A]): Lista[A] =
    xs match {
      case Nil => Nil
      case Cons(h,t) => append( reverse(t), Cons(h, Nil))
    }

  def reverseFL[A](xs: Lista[A]): Lista[A] = foldLeft(xs, Nil:Lista[A])((b,a) => append(Cons(a, Nil),b))

  def appendFR[A](xs1: Lista[A], xs2: Lista[A]): Lista[A] = foldRight(xs1, xs2)( (a,b) => Cons(a,b ) )
  def appendFL[A](xs1: Lista[A], xs2: Lista[A]): Lista[A] = foldLeft(xs1, xs2)( (b,a) => Cons(a,b))

  def transform(xs: List[Int]): List[Int] =
    xs match {
      case Nil => Nil
      case Cons(h,t) => Cons(h + 1, transform(t))
    }

}

val lst = Lista.apply(11,3,5,6,5,67,4,3) //Cons(1, Cons(3, Cons(5, Cons(6, Nil))))
val ls1 = Lista(1,2,4,5)
val n = Nil
Lista.tail(lst)
Lista.setHead(4, ls1)
Lista.init(lst)
Lista.dropWhile1(lst)( a => a < 3) // no need to writer a: Int
val sum2 = Lista.foldRight(lst, 0)(_+_)
Lista.foldRight(lst, Nil: Lista[Int])(Cons(_,_))
Lista.length(lst)
Lista.length2(lst)
Lista.sumFL(lst)
Lista.lengthFL(lst)
Lista.productFL(ls1)
Lista.reverse(ls1)
Lista.reverseFL(ls1)