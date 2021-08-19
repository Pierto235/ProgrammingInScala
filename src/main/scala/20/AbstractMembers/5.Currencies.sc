// How abstract classes can be used in Scala

//Class Currency - first design

abstract class Currency {
  val amount: Long
  def designation: String
  override def toString: String = amount + " " + designation
  def +(that: Currency): Currency = ???
  def *(x: Double): Currency = ???
}
//class Dol(x: Long) extends Currency {
//  val amount = x
//  override def designation: String = "USD"
//  def +(that:  Currency): Currency = new Dol(this.amount + that.amount)
//  def *(x:Double): Currency = new Dol((this.amount * x).toLong)
//}
//
//class Eur(x: Long) extends Currency {
//  val amount = x
//  override def designation: String = "EUR"
//  def +(that:  Currency): Currency = new Dol(this.amount + that.amount)
//  def *(x:Double): Currency = new Dol((this.amount * x).toLong)
//}
//
//val d = new Dol(10)
//val e = new Eur(15)
//
//d.+(e) // this shouldn't be allowed


val d = new Currency {
  override val amount: Long = 79L
  override def designation = "USD"
}
// the same like

class Dol1 extends Currency {
  override val amount: Long = 79L
  override def designation = "USD"
}
val d1 = new Dol1

class Dol2(x: Long) extends Currency {
  override val amount: Long = x
  override def designation = "USD"
}
val d2 = new Dol2(79)

class Dol3(x: Long, s:String) extends Currency {
  override val amount: Long = x
  override def designation = s
}
val d3 = new Dol3(79, "USD")



val e = new Currency {
  override val amount: Long = 79L
  override def designation = "EUR"
}

// Looks Good for single currency


abstract class Dollar extends Currency {
  override def designation: String = "USD"
}
new Dollar {
  override val amount: Long = 12
}
abstract class Euro extends Currency {
  override def designation: String = "EUR"
}
// This unfortunately will allow me to add usd to eur
// and the return type will be currency

// So we would need more specialized version of currency which take a particular currency
//But I want to write add method + just once not each time when new currency is defined


// Generally if I dont know something (value or type) I make it abstract, so
abstract class AbstractCurrency {
  type Currency <: AbstractCurrency
  def make(amount:Long): Currency //factory method
  val amount:Long
  def designation: String
  override def toString: String = amount + " " + designation
  def +(that: Currency): Currency = ???
//  def + (that: Currency): Currency = new Currency{
//    val amount = this.amount + that.amount
//  }
  def *(x: Double): Currency = ???
}

abstract class Dollar extends AbstractCurrency {
  type Currency = Dollar
  override def designation: String = "USD"

}

abstract class CurrecyZone {
  type Currency <: AbstractCurrency
  def make(x: Long): Currency
  abstract class AbstractCurrency {
    val amount:Long
    def designation: String
    override def toString: String = amount + " " + designation
    def +(that: Currency): Currency = make(this.amount + that.amount)
    def *(x: Double): Currency = make(this.amount * x.toLong)
  }
}

object US extends CurrecyZone {
  abstract class Dollar extends AbstractCurrency {
    override def designation: String = "USD"
  }
  type Currency = Dollar
  def make(x: Long) = new Dollar {val amount = x}

}








