package WzorceProjektowe

import scala.io.Source

case class Weight(kilos: Int)

trait WeightDispenser {
  val kilos: Int
  val next: Option[WeightDispenser]

  def dispense(weight: Weight): Unit = {
    if(weight.kilos >= kilos) {
      val w = weight.kilos / kilos
      val left = weight.kilos % kilos
      System.out.println(s"Dispesing $w weight/s of $kilos.")
      if(left > 0) next.map(_.dispense(Weight(left)))
    } else {
      next.foreach(_.dispense(weight))
    }
  }

}


class WeightDispenser20(val next: Option[WeightDispenser]) extends WeightDispenser {
  override val kilos: Int = 20
}
class WeightDispenser10(val next: Option[WeightDispenser]) extends WeightDispenser {
  override val kilos: Int = 20
}
class WeightDispenser5(val next: Option[WeightDispenser]) extends WeightDispenser {
  override val kilos: Int = 5
}
class WeightDispenser2(val next: Option[WeightDispenser]) extends WeightDispenser {
  override val kilos: Int = 2
}


class WeightDispenserMachine {
  val weightDispenser: WeightDispenser = {
    val wd2 = new WeightDispenser2(None)
    val wd5 = new WeightDispenser5(Some(wd2))
    val wd10 = new WeightDispenser10(Some(wd5))
    new WeightDispenser20(Some(wd10))
  }


  def requestWeight(weight: Weight): Unit = {
    if(weight.kilos % 2 != 0) {
      System.err.println("The smallest nominal is 2 and we cannot satisfy your request.")
    } else {
      weightDispenser.dispense(weight)
    }
  }
}


object WeightLifting {

  def main(args: Array[String]): Unit = {
    val wd = new WeightDispenserMachine
    printHelp()
    Source.stdin.getLines().foreach {
    case line => processLine(line, wd)
    }
  }

  def printHelp():Unit = {
    System.out.println("Usage: ")
    System.out.println("1. Write weight to set...")
    System.out.println("2. Write EXIT to quit the application.")
  }

  def processLine(input: String, wd: WeightDispenserMachine): Unit = {
    input match {
      case "EXIT" =>
        System.out.println("Bye!")
        System.exit(0)
      case l =>
        try {
          wd.requestWeight(Weight(l.toInt))
          System.out.println("Thanks!")
        } catch {
          case _: Throwable =>
            System.err.println(s"Invalid input: $l.")
            printHelp()
        }
    }
  }

}
