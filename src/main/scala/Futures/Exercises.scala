package Futures

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Random, Success, Try}

object Exercises extends App {

  /*
  1) fulfil a future immediately with a value
  2) inSequence(fa, fb)
  3) first(fa, fb) => new future with the first value on the two future
  4) last(fa, fb) => new future with the last value on the two future
  5) retryUntil[T](action: () => Future[T], condition: T => Boolean): Future[T]
   */

  //1
  def fulfillImmediately[T](value: T):Future[T] = Future(value)

  //2
  def inSequence[A,B](first: Future[A], second: Future[B]): Future[B] = {
    for {
      _ <- first
    } yield {second}

    first.flatMap{ _ => second.map{s => s}}

    first.flatMap{ _ => second}
  }

  //3
  def first[A,B](fa: Future[A], fb: Future[A]): Future[A] = {
    val promise = Promise[A]

    fa.onComplete(promise.tryComplete)
    fb.onComplete(promise.tryComplete)

    promise.future

  }

  //4
  def second[A,B](fa: Future[A], fb: Future[A]): Future[A] = {
    // 1 promise which both futures will try to complete
    // 2 promise which the LAST future will complete

    val bothPromise = Promise[A]
    val lastPromise = Promise[A]
    val checkAndComplete = (result:Try[A]) =>
      if(!bothPromise.tryComplete(result))
        lastPromise.complete(result)

    fa.onComplete(checkAndComplete)
    fb.onComplete(checkAndComplete)

    lastPromise.future

  }


  //5
  def retryUntil[A](action: () => Future[A], condition: A => Boolean): Future[A] =
    action()
      .filter(condition)
      .recoverWith {
        case _ =>  retryUntil(action, condition)
      }
  val random = new Random()
  val action = () => Future {
    Thread.sleep(100)
    val nextValue = random.nextInt(100)
    println("generated " + nextValue)
    nextValue
  }

  retryUntil(action, (x: Int) => x< 5).foreach(result => println("settled at " + result))
  Thread.sleep(10000)



}
