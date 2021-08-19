
import java.io.InputStreamReader
import com.github.tototoshi.csv.CSVReader
import org.json4s._
import org.json4s.native.JsonMethods

import scala.io.Source
import scala.xml.XML


object StrategyDesignPattern extends App {

  val lst = for( line <- Source.fromFile("eurofxref-hist.csv").getLines()) yield { line }


  val lstShortened = lst.toList
  val lstsRest = lstShortened.tail

  val lstOfCurrencies: List[String] = lstShortened.head.split(',').toList

  val lastLstElem = lstShortened.tail.last.split(',').toList

  val currencyListExtraction = lstsRest.map(elem => elem.split(',').toList)


  //println(lstOfCurrencies)
  //println(currencyListExtraction.head)


  case class ExchangesInDay( date: String,
                        mapOfExchanges: Map[String, String]
                      )

  val listOfExchangesInDay: List[ExchangesInDay] = currencyListExtraction.map( el => ExchangesInDay(el.head, (lstOfCurrencies.tail zip el.tail).toMap))


  //println(listOfExchangesInDay)

  def getRatesByDate(date: String):  Option[Map[String, String]] = listOfExchangesInDay.find(elem =>(elem.date == date)).map(_.mapOfExchanges)

  println(getRatesByDate("2021-05-11"))


  def getAmountByDate(date: String, currencyIn: String, currencyOut: String, amountToExchange: Int): Double = {
    getRatesByDate(date) match {
      case Some(map) => {
        val exchangeIn = map(currencyIn).toDouble
        val exchangeOut = map(currencyOut).toDouble
        amountToExchange * exchangeOut / exchangeIn
      }
      case None => 0
    }
  }

  println(getAmountByDate("2021-05-11", "USD", "GBP", 10))


  def getTheHighestReferance(startDate: String, endDate: String, currency: String): Double = {

    val daysBetweenDates = listOfExchangesInDay.filter(el => el.date>startDate && el.date < endDate)

    daysBetweenDates.map(d => d.mapOfExchanges(currency)).max.toDouble

  }


  def getTheAverageForPeriod(startDate: String, endDate: String, currency: String): Double = {

    val daysBetweenDates = listOfExchangesInDay.filter(el => el.date>startDate && el.date < endDate)

    val lst = daysBetweenDates.filter(el => el.mapOfExchanges(currency).nonEmpty)

    lst.map(d => d.mapOfExchanges(currency).toDouble).sum / lst.length

  }


//  case class Person(name: String, age: Int, address: String)
//
//  trait Parser[T] {
//    def parse(file: String): List[T]
//  }
//
//
//  class CSVParser extends Parser[Person] {
//
//    override def parse(file: String): List[Person] =
//      CSVReader.open(new
//          InputStreamReader(this.getClass.getResourceAsStream(file))).all().map {
//        case List(name, age, address) =>
//          Person(name, age.toInt, address)
//      }
//
//  }
//
//  class JsonParser extends Parser[Person] {
//    implicit val formats = DefaultFormats
//    override def parse(file: String): List[Person] =
//      JsonMethods.parse(StreamInput(this.getClass.getResourceAsStream(file))).extract[List[Person]]
//  }
//
//
//
//
//
//  object Parser {
//    def apply(filename: String): Parser[Person] =
//      filename match {
//        case f if f.endsWith(".json") => new JsonParser
//        case f if f.endsWith(".csv") => new CSVParser
//        case f => throw new RuntimeException(s"Unknown format: $f")
//      }
//  }
//
//  class PersonApplication[T](parser: Parser[T]) {
//    def write(file: String): Unit = {
//      println(s"Got the following data ${parser.parse(file)}")
//    }
//  }
//
//  val csvPeople: Parser[Person] = Parser("people.csv")
//  val jsonPeople: Parser[Person] = Parser("people.json")
//
//  val applicationCsv = new PersonApplication(csvPeople)
//  val applicationJson = new PersonApplication(jsonPeople)
//
//  println("Using the csv: ")
//  applicationCsv.write("people.csv")
//  println("Using the json: ")
//  applicationJson.write("people.json")
//
//  val musicElem = scala.xml.XML.loadFile("src/main/resources/music.xml")
//  println(musicElem)
//
//
//  case class Song(val title: String, val length: String) {
//    lazy val time = {
//      val Array(minutes, seconds) = length.split(":")
//      minutes.toInt*60 + seconds.toInt
//    }
//  }
//
//  case class Album(val title: String, val songs: Seq[Song], val description: String) {
//    lazy val time = songs.map(_.time).sum
//    lazy val length = (time / 60)+":"+(time % 60)
//  }
//
//  case class Artist(val name: String, val albums: Seq[Album])
//
//
//
//  val songs = (musicElem \\ "song").map { song =>
//    Song((song \ "@title").text, (song \ "@length").text)
//  }
//
//  val artists = (musicElem \ "artist").map { artist =>
//    val name = (artist \ "@name").text
//    val albums = (artist \ "album").map { album =>
//      val title = (album \ "@title").text
//      val description = (album \ "description").text
//      val songList = (album \ "song").map { song =>
//        Song((song \ "@title").text, (song \ "@length").text)
//      }
//      Album(title, songList, description)
//    }
//    Artist(name, albums)
//  }
//
//  val albumLengths = artists.flatMap { artist =>
//    artist.albums.map(album => (artist.name, album.title, album.length))
//  }
//  albumLengths.foreach(println)


}

