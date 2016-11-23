/**
  Class for operations on the weather infomation in APRS packets
  */

package net.hcoop.smallory.freezewarn

import scala.util.matching.Regex
import scala.collection.mutable.{Map, HashMap}

class aprsWeather {
  import net.hcoop.smallory.freezewarn.{aprsWeather => our}
  // This name will be used to access weather observation fields in
  // many places. Central to project, gets a nice short name.
  var wx: Map[String, Int] = null
  override def toString(): String = {
    return s""
  }
  def get(key: String): Option[Int] = {
    if (wx.contains(key)) return Some(wx(key))
      else return None
  }
}
object aprsWeather {
  // Patterns compiled during construction, so no extra compile step
  val wxRegex = new Regex(
    """^([0-9.]{3}|   )/([0-9.]{3}|   )g([0-9.\-]{3}|   )t([0-9.\-]{3}|   )([a-zA-Z0-9\-\. ]*)""",
    "c", "s", "g", "t","more")
  val compressedWxRegex = new Regex(
    """g([0-9.]{3}|   )t([0-9.-]{3}|   )([a-zA-Z0-9\-\. ]*)""", // possible ending is not matched
    "g", "t", "more")
  val wx2digitRegex = new Regex("""([h])([0-9-\.]{2}|  )""", "key", "value")
  val wx3digitRegex = new Regex("""([a-zA-Z&&[^hb]])([0-9-\.]{3}|   )""", "key", "value")
  val wx5digitRegex = new Regex("""([b])([0-9-\.]{5}|     )""", "key", "value")
  val missingValueRegex = """^(( *)||(\.)*)$""".r

  def apply(message: String) = {
    val w = new aprsWeather()
    w.wx = parseWeather(message)
    w
  }

  def parseWeather(payload: String): Map[String, Int] = {
    var m: Map[String, Int] = Map()
    // uncompressed most common, try first
    var wxSearch = wxRegex findFirstMatchIn payload
    if (wxSearch != None) {
      val wxMatch = wxSearch.get
      for (l <- "scgt") {
        val v = wxMatch.group(l.toString)
        if ((missingValueRegex findFirstIn v) == None) m += (l.toString -> v.toInt)
      }
      if (wxMatch.group("more").length > 2) m = m ++ getMoreWx(wxMatch.group("more"))
      return m
    }
    wxSearch = compressedWxRegex findFirstMatchIn payload
    if (wxSearch != None) {
      val wxMatch = wxSearch.get
      var m = Map(
        ("s", wxMatch.group("s").toInt),
        ("c", wxMatch.group("c").toInt),
        ("g", wxMatch.group("g").toInt),
        ("t", wxMatch.group("t").toInt)
      )
      if (wxMatch.group("more").length > 2) m = m ++ getMoreWx(wxMatch.group("more"))
      return m
    }
    // return empty map upon failure
    m
  }

  def getMoreWx(moredat: String): Map[String, Int] = {
    var m: Map[String, Int] = Map()
    for (rex <- List(wx2digitRegex, wx3digitRegex, wx5digitRegex)) {
      for (mm <- rex findAllMatchIn moredat) {
        val vv = mm.group("value")
        if ((missingValueRegex findFirstIn vv) == None) m += (mm.group("key") -> vv.toInt)
      }
    }
    return m
  }

  // Using direct tuple notation instead of arrow notation, for the cleans.
  val weatherFields = Map(
    ("g", (3, "mph", "peak wind gust")),
    ("c", (3, "degrees", "wind direction")),
    ("s", (3, "mph", "1-minute sustained wind")),
    ("t", (3, "farenheit", "temperature")),
    ("r", (3, "1/100 inch","rainfall in last hour")),
    ("p", (3, "1/100 inch","rainfall in last 24 hours")),
    ("P", (3, "1/100 inch","rainfall since midnight")),
    ("h", (2, "percent", "humidity")),
    ("b", (5, "1/10 millibars", "barametric pressure")),
    ("L", (3, "W/m^2", "of luminosity")),
    ("l", (3, "W/m^2", "of luminosity minus 1000 W/m^2")),
    ("s", (3, "in", "new snow in last 24 hours")),
    ("#", (3, "count", "raw count of rain detector")),
    ("F", (3, "feet", "height of water above reference")),
    ("f", (3, "meters", "height of water above reference")),
    ("V", (3, "volts", "charge on battery")),
    ("Z", (2, "", "device type")),
    ("X", (3, "nanosieverts", "radiation, as xxy = xx*10^y nanosieverts/hour"))
   )
}
