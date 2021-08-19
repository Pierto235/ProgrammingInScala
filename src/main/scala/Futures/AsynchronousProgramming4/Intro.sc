import scala.concurrent.{ExecutionContext, Future}

def getWebpage(url: String): String = ???

def getWebpage(url: String): Future[String] = ???

/**
Note
The Future[T] type encodes latency in the program; use it to encode values that will become available later during execution.
 */

trait Future[T]

def apply[T](b: =>T)(implicit e: ExecutionContext): Future[T]

