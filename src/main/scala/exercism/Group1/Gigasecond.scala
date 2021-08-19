package exercism.Group1

import java.time.LocalDate
import java.time.LocalDateTime

object Gigasecond {

  private val gigasecond = Math.pow(10, 9)

  def add(date: LocalDate):LocalDateTime =
    date.atStartOfDay().plusSeconds(gigasecond.toInt)

  def add(date: LocalDateTime):LocalDateTime =
    date.plusSeconds(gigasecond.toInt)

}
