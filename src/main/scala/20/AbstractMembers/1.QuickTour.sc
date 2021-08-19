// trait Abstract with 4 abstract members (declarations)
trait Abstract {
  type T // abstract type - they are always members of some class or traits
  def transform(x: T): T
  val initial: T
  var current: T
}

// definitions must be supplied in concrete classes

class ConcreteString extends Abstract {
  type T = String // alias for the type T
  def transform(x: T) = x + x
  val initial = "hi"
  var current = initial
}

class ConcreteInt extends Abstract {
  type T = Int
  def transform(x: T) = x * x
  val initial = 1
  var current = initial
}

case class Person(name: String, age: Int)
class ConcreteChar extends Abstract {
  type T = Person
  def transform(x: T) = Person(x.name, x.age + 1)
  val initial = Person("", 0)
  var current = initial
}

//1.
// The other main use of type members is to declare abstract types that must be defined in subclasses.

//2.
// You use an abstract val declaration in a class when you dont know the correct value in the class, but
// you do know that the variable will have an unchangeable value in each instance of the class.
// - val always returns the same value as oppose to def initial (can not be changed in subclass into def or var)
// - def could be changed into val or var


// Abstract vars

trait AbstractTime {
  var hour: Int
  var minute: Int
}

// this is equivalent to
// var declared as members of classes come equipped with get and set method

trait AbstractTime {
  def hour: Int
  def hour_=(x:Int)
  def minute: Int
  def minute_=(x: Int)
}



