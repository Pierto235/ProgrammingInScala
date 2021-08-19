import scala.annotation.tailrec

def findFirst(ss: Array[String], key: String): Int = {
  @tailrec
  def loop(n: Int): Int = {
    if( n > ss.length) - 1
    else {
      if(ss(n) == key) n
      else loop(n+1)
    }
  }
  loop(0)
}
findFirst(Array("a", "b", "c"), "b")


def findFirst1[A](as: Array[A], p: A => Boolean): Int = {
  @tailrec
  def loop(n: Int): Int = {
    if( n > as.length) - 1
    else {
      if (p(as(n))) n
      else loop(n+1)
    }
  }
  loop(0)
}

def p(x: Int): Boolean = x == 2
def r(x: String): Boolean = x == "b"

findFirst1(Array(1,2,3), (x: Int) => x == 1)
findFirst1(Array("a", "b", "c"), r)


def orderedInt(i1: Int, i2: Int): Boolean = i1 <= i2

(i1: Int, i2: Int) => i1 < i2

val orderedInt1 = new Function2[Int, Int, Boolean] {
  def apply(a: Int, b: Int): Boolean = a < b
}
orderedInt1.apply(1,2)

def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  if(as.length <= 1) true
  else {
    def loop(n: Int): Boolean = {
      if(ordered(as(n-1), as(n))) {
        if(n+1 == as.length) true
        else loop(n + 1)
      }
      else false
    }
    loop(1)
  }
}

def isSorted2[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if(n >= as.length - 1) true
      else {
        if(ordered(as(n), as(n+1))) loop(n+1)
        else false
      }
    }
    loop(0)
}

isSorted(Array(2,1), orderedInt)

/////////////////////
def partial1[A, B, C](a: A, f:(A,B) => C): B => C =
  (b: B) => f(a,b)

val p = partial1(2, (a: Int,b:Int) => a+b)
p(5)
/////////////////////////

def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  (a: A) => (b: B) => f(a,b)
val c = curry((a: Int, b: Int) => a+b)
c(1)(2)

///////////////////////////

def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a: A, b: B) => f(a)(b)
val uc = uncurry((a:Int) => (b: Int) => a+b )
uc(1,2)

///////////////////////////

def compose[A, B, C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))

val com = compose((a:Int) => 2*a, (a: Int) => 4*a)
com(1)

















