package WzorceProjektowe

import scala.collection.mutable.ListBuffer

// Symulacja wykonywanych komend podczas lotu

trait CC {
  def przyjmijPasazerow():Unit
  def wykonajSOS(): Unit
  def zrobSerwis(): Unit
  def pozbierajSmieci(): Unit = println("CC zbiera smieci")
}

case class CCTom() extends CC {
  override def przyjmijPasazerow():Unit = println("Tom przyjmuje pasazerow w tyle kabiny")
  override def wykonajSOS(): Unit = println("Tom wykonuje Sos w rzedzie 21")
  override def zrobSerwis(): Unit = println("Tom robi serwis z Julie")
}


case class CCJulie() extends CC {
  override def przyjmijPasazerow():Unit = println("Julie przyjmowania pasazerow w srodku kabiny")
  override def wykonajSOS(): Unit = println("Julie wykonuje Sos w rzedzie 11")
  override def zrobSerwis(): Unit = println("Julie robie serwis z Tomem")
}


trait Komenda {
  def wykonaj(): Unit
}

case class PrzyjmijPasazerow(cc: CC) extends Komenda {
  override def wykonaj(): Unit = cc.przyjmijPasazerow()
}

case class WykonajSos(cc: CC) extends Komenda {
  override def wykonaj(): Unit = cc.wykonajSOS()
}

case class ZrobSerwis(cc: CC) extends Komenda {
  override def wykonaj(): Unit = cc.zrobSerwis()
}

case class ZbierajSmieci(cc: CC) extends Komenda {
  override def wykonaj(): Unit = cc.pozbierajSmieci()
}



class CSSKontroller {
  def wyslijKomende(komenda: Komenda): Unit = {
    komenda.wykonaj()
  }
}


object TheCommandDesignPractice {

  def main(args: Array[String]):Unit = {

    val lotCssAshley = new CSSKontroller
    val ccTom =  CCTom()
    val ccJulie =  CCJulie()

    println("Przyjmowanie pasazerow na początku zmiany..")
    lotCssAshley.wyslijKomende(PrzyjmijPasazerow(ccTom))
    lotCssAshley.wyslijKomende(PrzyjmijPasazerow(ccJulie))

    println("\nSOS przed rozpoczeciem lotu...")
    lotCssAshley.wyslijKomende(WykonajSos(ccTom))
    lotCssAshley.wyslijKomende(WykonajSos(ccJulie))
    println("\nSerwis po 20 min lotu...")
    lotCssAshley.wyslijKomende(ZrobSerwis(ccTom))
    lotCssAshley.wyslijKomende(ZrobSerwis(ccJulie))
    println("\nPo serwisie...")
    lotCssAshley.wyslijKomende(ZbierajSmieci(ccTom))


  }



}
