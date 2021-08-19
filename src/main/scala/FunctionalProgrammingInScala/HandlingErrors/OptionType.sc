
import scala.util.Try
//
//trait Options[+A] {
//  def map[B](f: A => B): Options[B]
//  def flatMap[B](f: A => Options[B]): Options[B]
//  def getOrElse[B>:A](default: => B): B
//  def orElse[B>: A](ob: => Options[B]): Options[B]
//  def filter(f: A => Boolean): Options[A]
//}
//case object None extends Options[Nothing] {
//  override def map[B](f: Nothing => B): Options[B] = None
//  def flatMap[B](f: Nothing => Options[B]): Options[B] = None
//  def getOrElse[B>:Nothing](default: => B): B = default
//  def orElse[B>: Nothing](ob: => Options[B]): Options[B] = ob
//  def filter(f: Nothing => Boolean): Options[Nothing] = None
//}
//case class Some[+A](get: A) extends Options[A] {
//  override def map[B](f: A => B): Options[B] = Some(f(get))
//  def flatMap[B](f: A => Options[B]): Options[B] = f(get)
//  def getOrElse[B>:A](default: => B): B = get
//  def orElse[B>: A](ob: => Options[B]): Options[B] = this
//  def filter(f: A => Boolean): Options[A] = if(f(get)) this else None
//}

val a = Some(5)
val a1 = None
a.map(a=>2*a)
a1.map(a=>a)
a1.map(_)

a.flatMap(a => Some(2*a))
a1.flatMap(a=>a)

a.orElse(Some(0))

case class Department(name:String, manager: Option[String])
case class Employee(name: String, department: Department)

val lstOfEmployees = List(Employee("Tom", Department("It", Some("Tom"))), Employee("Ada", Department("sales", Some("Gary"))),
  Employee("Ula", Department("transport", None)))

def lookupByName(name: String): Option[Employee] = lstOfEmployees.find(e => e.name == name)

val tomsDep = lookupByName("Tom").map(_.department)
val mikeDep = lookupByName("Mike").map(_.department)

val tomsManager = lookupByName("Tom").map(_.department).flatMap(_.manager)
val ulasManager = lookupByName("Ula").map(_.department).flatMap(_.manager)
val ulasManagerDefault = lookupByName("Ula").map(_.department).map(_.manager.getOrElse("Bob"))


def mean(xs: Seq[Double]): Option[Double] =
  if( xs.isEmpty) None else Some(xs.sum/xs.length)

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(m => Some(xs.map(x => math.pow(x-m,2)).sum / xs.length))


variance(Seq())
variance(List(1,2,3))


// ------------------ LIFT -----------------------------

def lift[A,B](f:A=>B): Option[A] => Option[B] = _ map f

val abs: Double => Double = math.abs
def abs1(x: Double): Double = math.abs(x)
val abs0: Option[Double] => Option[Double] = lift(math.abs)


/////////////////////////////////////////////////////////////

def Try[A](a: => A): Option[A] =
  try Some(a)
  catch { case e: Exception => None}

def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B)=>C): Option[C] = {
  a flatMap (aa => b map (bb => f(aa, bb)))
//  (a, b) match {
//    case (Some(a), Some(b)) => Some(f(a,b))
//    case _ => None
//  }
}

def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
  val optAge: Option[Int] = Try(age.toInt)
  val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
  map2(optAge, optTickets)(insuranceRateQuote)
}

// List( Some(a), Some(b), Some(c) ) => Some(List(a, b, c))
def sequence[A](a: List[Option[A]]): Option[List[A]] =
  a match {
    case Nil => Some(Nil)
    case h::t => h flatMap ( hh => sequence(t) map(hh::_) )
  }













