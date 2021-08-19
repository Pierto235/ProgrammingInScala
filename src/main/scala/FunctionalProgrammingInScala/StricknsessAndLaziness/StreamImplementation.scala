package FunctionalProgrammingInScala.StricknsessAndLaziness

import scala.annotation.tailrec

/**
 * Implement a lazily evaluated, singly linked Stream of elements.
 *
 * naturals = MyStream.from(1)(x => x+1) =  stream of natural numbers (potentially infinite!)
 * naturals.take(100).foreach(println)  // lazily evaluated stream of the first 100 natural numbers (finite stream)
 * naturals.foreach(println)  // will crash - infinite!
 * naturals.map(_ * 2) // stream of all even numbers (potentially infinite)
 *
 */

abstract class MyStream[+A] {
  def isEmpty: Boolean
  def head:A
  def tail: MyStream[A]

  def #::[B >: A](element: B): MyStream[B]
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B]

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def filter(predicate: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A]
  def takeAsList(n: Int): List[A] = take(n).toList()

  @tailrec
  final def toList[B >: A](acc: List[B] = Nil): List[B] =
    if (isEmpty) acc.reverse
    else tail.toList(head :: acc)

}

object EmptyStream extends MyStream[Nothing] {
  def isEmpty: Boolean = true
  def head:Nothing = throw new NoSuchElementException
  def tail: MyStream[Nothing] = throw new NoSuchElementException

  def #::[B >: Nothing](element: B): MyStream[B] = new Cons(element, this)
  def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream

  def foreach(f: Nothing => Unit): Unit = ()
  def map[B](f: Nothing => B): MyStream[B] = this
  def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this
  def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this

  def take(n: Int): MyStream[Nothing] = this

}

class Cons[+A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
  def isEmpty: Boolean = false
  override val head: A = hd
  override lazy val tail: MyStream[A] = tl

  // val s = new Cons(1, EmptyStream)
  // val prepended = 1 #:: s = new Cons( 1, s )
  def #::[B >: A](element: B): MyStream[B] = new Cons[B](element, this)

  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = new Cons[B](head, tail ++ anotherStream)

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  // s = new Cons(1, ?)
  //mapped = s.map(_+1) = new Cons(2, s.tail.map(_+1)
  // ... mapped.tail
  def map[B](f: A => B): MyStream[B] = new Cons[B](f(head), tail.map(f)) //preserves lazy evaluation
  def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)
  def filter(predicate: A => Boolean): MyStream[A] =
    if(predicate(head)) new Cons(head, tail.filter(predicate))
    else tail.filter(predicate)  //preserves lazy evaluation

  def take(n: Int): MyStream[A] = {
    if( n > 0) new Cons(head, tail.take(n-1))
    else EmptyStream
  }

  
}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] =
    new Cons(start, from(generator(start))(generator))
}

object StreamImplementation extends App {

  val naturals = MyStream.from(1)(_+1)

  println(naturals.head)
  println(naturals.tail.head)
  println(naturals.tail.tail.head)

  println

  val startFrom0 = 0 #:: naturals
  println(startFrom0.head)

  println

  startFrom0.take(10).foreach(println)

  //

  println(startFrom0.map(_*2).take(100).take(100).toList())

  println(startFrom0.flatMap(el => new Cons(el*2, EmptyStream )).take(100).toList())

  println(startFrom0.filter(_ %2 == 0).take(10).toList())

  //Exercises

  //1. stream of fibonacci numbers
  //2. stream of prime numbers with Eratosthenas sieve

  def fibonacci(n: Int): List[Int] = {
    n match {
      case n if n < 0 => Nil
      case 0 => List(0)
      case 1 => List(0, 1)
      case m => {
        def helper(acc: List[Int], counter: Int, last: Int): List[Int] = {
          if(counter == m) (last::acc).reverse
          else helper(last :: acc, counter + 1, last + acc.head)
        }
        helper(List(1, 0), 2, 1)
      }
    }
  }
  println(fibonacci(3))



  def fibo(n: Int): List[Int] = {
    def helper(acc: List[Int], counter: Int): List[Int] = {
      if(counter == n + 1) Nil
      else {
        val num = acc(counter-1) + acc(counter-2)
        num::helper(acc:::List(num), counter+1)
      }

    }
    List(0,1):::helper(List(0,1), 2)

  }
  println(fibo(5))


  def fiboStream: Stream[Int] = {
    def helper(acc: Stream[Int], counter: Int): Stream[Int] = {
      val num = acc(counter-1) + acc(counter-2)
      num#::helper(acc++Stream(num), counter+1)
    }
    Stream(0,1)++helper(Stream(0,1), 2)
  }
  println(fiboStream.take(5).toList)


  def fibonacciMs(first: Int, second: Int): MyStream[Int] =
    new Cons[Int](first, fibonacciMs(second, first + second))

  println(fibonacciMs(1, 1).take(10).toList())



  def sieve(limit: Int): List[Int] = {
    val lst = 2 to limit toList

    def helper(acc: List[Int], reduced: List[Int]): List[Int] = {
      reduced match {
        case Nil => acc.reverse
        case h:: tail => helper(h::acc, tail.filter(el => el%h != 0))
      }
    }
    helper(Nil, lst)

  }
  println(sieve(13))



  def erystothanes(numbers: MyStream[Int]): MyStream[Int] = {

    def helper(acc: MyStream[Int], reduced: MyStream[Int]): MyStream[Int] = {
      if(reduced.isEmpty) acc
      else new Cons( reduced.head, helper( reduced.head #:: acc, reduced.tail.filter(el => el%reduced.head != 0)))
    }
    helper(EmptyStream, numbers)

  }


  println(erystothanes( MyStream.from(2)(_+1)).take(10).toList())

  def erystothanesBetter(numbers: MyStream[Int]): MyStream[Int] = {
      if(numbers.isEmpty) numbers
      else new Cons( numbers.head, erystothanes(numbers.tail.filter(el => el%numbers.head != 0))  )
    }

  println(erystothanesBetter( MyStream.from(2)(_+1)).take(10).toList())

}
