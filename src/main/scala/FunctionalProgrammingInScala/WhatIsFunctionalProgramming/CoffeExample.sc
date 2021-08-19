
/** A program with side effects */

class CreditCard {
  var amount: Double = 500
  def charge(p: Double):Unit = {
    amount = amount - p
  }
}
class Coffee(){
  val price: Double = 2.0
}

class Payments() {
  def charge(cc: CreditCard, p: Double): Unit = ???
}

case class Charge(cc: CreditCard, amount: Double){

  def combine( other : Charge): Charge = {
    if(cc == other.cc) Charge(cc, amount + other.amount)
    else throw new Exception("Can't combine charges to different cards")
  }

}

class Cafe {

  def buyCoffee( cc: CreditCard): Coffee = {
    val cup = new Coffee()
    cc.charge(cup.price) // side effect - communication with outside word (contacting cc company, authorizing, charging, keeping track of transaction,...)
    cup                 // but function only returns a coffee
  } // this function is difficult to test since we need to contact cc company and do the other tings like  authorizing, ...


  def buyCoffee1( cc: CreditCard, p: Payments): Coffee = {
    val cup = new Coffee()
    p.charge(cc, cup.price) // side effect still occur but Credit Card is now more modular and testable (i.e by writing mock test for Payments)
    cup
  }
  // but really we just want to test that buyCoffee creates a charge equal to the price of a cup of coffee.
  // also it creates problem if we want to order more than one coffee

}

/** Functional solution -  removing side effects */

class Cafe1 {
  def buyCoffee( cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee()
    ( cup , Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip

    (coffees, charges.reduce((c1,c2) => c1.combine(c2)))
  }

}



val cc1 = new CreditCard
val cc2 = new CreditCard
val charges: List[Charge] = List(Charge(cc1, 2), Charge(cc1,3), Charge(cc1,2), Charge(cc2,5), Charge(cc2,6))

charges.groupBy(_.cc).values











