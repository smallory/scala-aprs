/**
  Package utilities
  */
package net.hcoop.smallory
import java.time.{ZoneOffset, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.time.format.FormatStyle.{FULL, LONG, MEDIUM, SHORT}
import scala.collection.mutable.Map

package object scalaprs{
  type ObservationMap = scala.collection.mutable.Map[String, Float]

  val utc = ZoneOffset.UTC

  // Making certain that it's easy to change loggers.
  var logIgnore: List[String] = List()
  def logAs(errType: String, message: String) = {
    if (!logIgnore.contains(errType))
      println( errType.toUpperCase + " @ " +
        ZonedDateTime.now.format(
          DateTimeFormatter.ofLocalizedDateTime(SHORT, SHORT)) +
        ":" + message)
  }
  val logNote = logAs("note", _: String)
  val logInfo = logAs("note", _: String)
  val logWarn = logAs("warning", _: String)
  val logCodeError = logAs("error", _: String)
  val logDataError = logAs("data", _: String)
  val logDebug = logAs("debug", _: String)

  def base91decode(str: String): Long = {
    var l: Long = 0
    for (c <- str) {
      l *= 91
      l += c - 33
    }
    return l
  }

  def base91encode(num: Long): String = {
    var s: String = ""
    var n: Long = num
    while (n > 0) {
      s = ((n % 91) + 33).toChar + s
      n = n/91
    }
    return s
  }

  def mapFromStrings(arFile: Iterable[String]): Map[String, String] = {
    var rtrn: Map[String, String] = Map()
    rtrn ++= arFile map { (entry: String) =>
      val field = entry.split("=")
      if (field.size > 1) Some(field(0) -> field(1))
      else None
    } collect ({case Some((x:String, y:String)) => (x -> y)})
    return rtrn
  }
}

