import scala.io.Source

case class Money(amount: Int)

trait Dispenser {
  val amount: Int
  val next: Option[Dispenser]

  def dispense(money: Money): Unit = {
    if(money.amount >= amount) {
      val notes = money.amount / amount
      val left = money.amount % amount
      System.out.println(s"Dispesing $notes note/s of $amount.")
      if(left > 0) next.map(_.dispense(Money(left)))
    } else {
      next.foreach(_.dispense(money))
    }
  }
}

class Dispenser50(val next: Option[Dispenser]) extends Dispenser {
  override val amount: Int = 50
}
class Dispenser20(val next: Option[Dispenser]) extends Dispenser {
  override val amount: Int = 20
}
class Dispenser10(val next: Option[Dispenser]) extends Dispenser {
  override val amount: Int = 10
}
class Dispenser5(val next: Option[Dispenser]) extends Dispenser {
  override val amount: Int = 5
}

class ATM {
  val dispenser: Dispenser = {
    val d1 = new Dispenser5(None)
    val d2 = new Dispenser10(Some(d1))
    val d3 = new Dispenser20(Some(d2))
    new Dispenser50(Some(d3))
  }

  def requestMoney(money: Money): Unit = {
    if(money.amount % 5 != 0) {
      System.err.println("The smallest nominal is 5 and we cannot satisfy your request.")
    } else {
      dispenser.dispense(money)
    }
  }
}


object ATMExample {

  def main(args: Array[String]): Unit = {
    val atm = new ATM
    printHelp()
    Source.stdin.getLines().foreach{
      case line => processLine(line, atm)
    }
  }

  def printHelp():Unit = {
    System.out.println("Usage: ")
    System.out.println("1. Write amount to withdraw...")
    System.out.println("2. Write EXIT to quit the application.")
  }

  def processLine(line: String, atm: ATM): Unit = {
    line match {
      case "EXIT" =>
        System.out.println("Bye!")
        System.exit(0)
      case l =>
        try {
          atm.requestMoney(Money(l.toInt))
          System.out.println("Thanks!")
        } catch {
          case _: Throwable =>
            System.err.println(s"Invalid input: $l.")
            printHelp()
        }
    }
  }


}
