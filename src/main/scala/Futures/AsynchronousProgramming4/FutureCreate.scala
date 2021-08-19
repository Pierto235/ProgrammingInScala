package Futures.AsynchronousProgramming4

import scala.concurrent._
import ExecutionContext.Implicits.global


object FuturesCreate extends App {

  Future { println("the future is here") }
  println("the future is coming")
  Thread.sleep(1000)

}

///////////////////////////////////////////

import scala.io.Source
object FuturesDataType extends App {

  val buildFile: Future[String] = Future {
    val f = Source.fromFile("build.sbt")
    try f.getLines.mkString("\n") finally f.close()
  }

  println(s"started reading the build file asynchronously")
  println(s"status: ${buildFile.isCompleted}")
  Thread.sleep(250)
  println(s"status: ${buildFile.isCompleted}")
  println(s"value: ${buildFile.value}")
}

//////////////////////////////////////////

object FuturesCallbacks extends App {
  def getUrlSpec(): Future[List[String]] = Future {
    val url = "http://www.w3.org/Addressing/URL/url-spec.txt"
    val f = Source.fromURL(url)
    try f.getLines.toList finally f.close()
  }
  val urlSpec: Future[List[String]] = getUrlSpec()

  def find(lines: List[String], keyword: String): String = lines.zipWithIndex collect {
    case (line, n) if line.contains(keyword) => (n, line)
  } mkString("\n")

  urlSpec foreach {
    case lines => println(find(lines, "telnet"))
  }

  println("callback registered, continuing with other work")

  Thread.sleep(2000)
  urlSpec foreach {
    case lines => println(find(lines, "password"))
  }
  Thread.sleep(1000)
}







