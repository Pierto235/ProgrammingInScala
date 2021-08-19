package Futures.AsynchronousProgramming4

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}


object FuturesFailure extends App {
  val urlSpec: Future[String] = Future {
    val invalidUrl = "http://www.w3.org/non-existent-url-spec.txt"
    Source.fromURL(invalidUrl).mkString
  }
  urlSpec.failed foreach {
    case t => println(s"exception occurred - $t")
  }
  Thread.sleep(1000)
}
////////////////////////////////////////


object FuturesTry extends App {

  def handleMessage(t: Try[String]) = t match {
    case Success(msg) => println(msg)
    case Failure(error) => println(s"unexpected failure - $error")
  }

  val threadName: Try[String] = Try(Thread.currentThread.getName)
  val someText: Try[String] = Try("Try objects are synchronous")
  val message: Try[String] = for {
    tn <- threadName
    st <- someText
  } yield s"Message $st was created on t = $tn"
  handleMessage(message)
}

