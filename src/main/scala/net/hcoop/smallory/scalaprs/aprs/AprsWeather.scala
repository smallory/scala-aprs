/**
  Class for operations on the weather infomation in APRS packets
  */

package net.hcoop.smallory.scalaprs

import scala.util.matching.Regex
import scala.collection.mutable.{Map, HashMap}

class AprsWeather {
  import net.hcoop.smallory.scalaprs.{AprsWeather => our}
  // This name will be used to access weather observation fields in
  // many places. Central to project, gets a nice short name.
  var wx: ObservationMap = null

  def get(key: String): Option[Double] = {
    if (wx.contains(key)) return Some(wx(key))
      else return None
  }
  def getObs(key: String): Option[WxObservation] = {
    if (wx.contains(key)) {
      return Some(WxObservation(
        Double.NaN, Double.NaN, 0l, key, wx(key), our.weatherFields(key)._2 ))
    } else return None
  }
}
object AprsWeather {
  // Patterns compiled during construction, so no extra compile step
  val wxRegex = new Regex(
    """^([0-9.]{3}|   )/([0-9.]{3}|   )g([0-9.\-]{3}|   )t([0-9.\-]{3}|   )([a-zA-Z0-9\-\. ]*)""",
    "c", "s", "g", "t","more")
  val compressedWxRegex = new Regex(
    """([!-|][!-|][!-|])g([0-9.]{3}|   )t([0-9.-]{3}|   )([a-zA-Z0-9\-\. ]*)""", // possible ending is not matched
    "sc", "g", "t", "more")
  val wx3digitRegex = new Regex("""([a-zA-Z&&[^hblXfF]])([0-9-\.]{3}|   )""", "key", "value")
  val wx4digitRegex = new Regex("""([fF])([0-9-\.]{4}|    )""", "key", "value")
  val wx5digitRegex = new Regex("""([b])([0-9-\.]{5}|     )""", "key", "value")
  val wxHighLRegex = new Regex("""(l)([0-9-\.]{3}|   )""", "key", "value")
  val wxExponentRegex = new Regex("""([X])([0-9-\.]{3}|   )""", "key", "value")
  val wxPercentRegex = new Regex("""([h])([0-9-\.]{2}|  )""", "key", "value")
  val missingValueRegex = """^(( *)||(\.)*)$""".r

  def apply(message: String) = {
    val w = new AprsWeather()
    w.wx = parseWeather(message)
    w
  }

  def parseWeather(payload: String): ObservationMap = {
    var m: ObservationMap = Map()
    // uncompressed most common, try first
    var wxSearch = wxRegex findFirstMatchIn payload
    if (wxSearch == None) {
      wxSearch = compressedWxRegex findFirstMatchIn payload
      // This will break, since "s" and "c" are not numbers
    }
    if (wxSearch != None) {
      val wxMatch = wxSearch.get
      for (l <- "scgt") {
        val v = wxMatch.group(l.toString)
        if ((missingValueRegex findFirstIn v) == None) m += (l.toString -> v.toDouble)
      }
      if (wxMatch.group("more").length > 2) m = m ++ getMoreWx(wxMatch.group("more"))
      return m
    }
    // return empty map upon failure
    m
  }

  def getMoreWx(moredat: String): ObservationMap = {
    var m: ObservationMap = Map()
    // Grab-the-value regexen
    for (rex <- List(wx3digitRegex, wx4digitRegex, wx5digitRegex)) {
      for (mm <- rex findAllMatchIn moredat) {
        val vv = mm.group("value")
        if ((missingValueRegex findFirstIn vv) == None) m += (mm.group("key") -> vv.toDouble)
      }
    }
    // Special processing regexen
    for (rex <- List(wxHighLRegex, wxExponentRegex, wxPercentRegex)) {
      for (mm <- rex findAllMatchIn moredat) {
        val vv = mm.group("value")
        if ((missingValueRegex findFirstIn vv) == None) {
          val kk = mm.group("key")
          kk match {
            // percents
            case "h" => m += ( kk -> (if (vv == "00") 100d else vv.toDouble))
            // lux rollover
            case "l" => m += ( "L" -> (1000 + vv.toDouble))
            // Exponential measure expansion
            case "X" => m += ( kk ->
                ( (vv take 2).toDouble * (scala.math.pow(10, (vv drop 2 take 1).toDouble)) ).toDouble)
          }
        }
      }
    }
    return m
  }

  // Using direct tuple notation instead of arrow notation, for the cleans.
  val weatherFields = Map(
    ("g", (3, "mph", "peak wind gust")),
    ("c", (3, "degrees", "wind direction")),
    // Speed is mph in "stand alone" reports, but usually from
    // "7-byte DIR/SPD Data Extension" which provides knots.
    ("s", (3, "knots", "1-minute sustained wind")),
    ("t", (3, "F", "temperature")),
    ("r", (3, "1/100 inch","rainfall in last hour")),
    ("p", (3, "1/100 inch","rainfall in last 24 hours")),
    ("P", (3, "1/100 inch","rainfall since midnight")),
    ("h", (2, "percent", "humidity")),
    ("b", (5, "1/10 millibars", "barametric pressure")),
    ("L", (3, "W/m^2", "of luminosity")),
    ("l", (3, "W/m^2", "of luminosity minus 1000 W/m^2")),
    ("s", (3, "in", "new snow in last 24 hours")),
    ("#", (3, "count", "raw count of rain detector")),
    ("F", (4, "ft", "height of water above reference")),
    ("f", (3, "m", "height of water above reference")),
    ("V", (3, "v", "charge on battery")),
    ("Z", (2, "", "device type")),
    ("X", (3, "nSv", "radiation, as xxy = xx*10^y nanosieverts/hour"))
   )
}
