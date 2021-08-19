
sealed trait Either[+E, +A] {
  def map[B](f: A=>B): Either[E,B]
  def map2[EE >: E,B,C](b: Either[EE, B])(f:(A,B)=>C): Either[EE,C]
}
case class Left[+E](value: E) extends Either[E, Nothing]{
  def map[B](f: Nothing=>B): Either[E,B] = Left(value)
  def map2[EE >: E,B,C](b: Either[EE, B])(f:(Nothing,B)=>C): Either[EE,C] = ???
}
case class Right[+A](value: A) extends Either[Nothing, A]{
  def map[B](f: A=>B): Either[Nothing,B] = Right(f(value))

  def map2[EE >: E,B,C](b: Either[EE, B])(f:(A,B)=>C): Either[EE,C] = for{
    a <- value
    b1 <- b
  } yield f(a, b1)
}

def mean(xs: IndexedSeq[Double]): Either[String, Double] =
  if(xs.isEmpty) Left("mean of empty list")
  else Right(xs.sum/xs.length)

def safeDiv(x: Int, y: Int): Either[Exception, Int] =
  try Right(x/y)
  catch { case e: Exception => Left(e) }

def Try[A](a: => A): Either[Exception, A] =
  try Right(a)
  catch { case e: Exception => Left(e) }



case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

def mkName(name: String): Either[String, Name] =
  if(name == "" || name == null) Left("Name is empty")
  else Right(new Name(name))

def mkAge(age: Int): Either[String, Age] =
  if(age<0) Left("Age is out of range.")
  else Right(new Age(age))

def mkPerson(name: String, age: Int): Either[String, Person] =
  mkName(name).map2(mkAge(age))(Person(_,_))