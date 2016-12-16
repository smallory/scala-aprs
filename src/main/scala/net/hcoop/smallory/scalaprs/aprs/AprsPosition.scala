/**
  Class for operations on the positions of APRS packets
  */

package net.hcoop.smallory.scalaprs

import scala.util.matching.Regex

/** Within APRS positions, per spec as DD mm.mm - decimal minutes, not degrees.
  * 
  *  These are string values, with spaces used to represent ambiguity.
  *  While representing this in String is aweful for manipulation,
  *  it works really well for processing the incoming text data.
  * 
  *  Another oddity is that the "symbol table" data is intimitely mixed
  *  with the location data.
  * 
  * APRS official "null" position and symbol are "\." at 0000.00N/0000.00W.
  * 
  * Alternate formats are locations in Maidenhead coordinates, compressed lat/lon,
  * NMEA stanzas, or Mic-E. With raw NMEA from a station GPS, tracking location by
  * station may be needed.  :(
  * 
  * This is from the APRS spec, and might be true of implementations:
  *  "The default GPS earth datum is World Geodetic System (WGS) 1984."
  *
  */
class AprsPosition {
  import net.hcoop.smallory.scalaprs.{AprsPosition => our}
  var lats: String = "0000.00N"
  var lons: String = "00000.00W"
  var error: Int = 6
  var maidenhead: String = null
  var table: String = "\\"
  var symbol: String = "."
  var latf: Double = 0d
  var lonf: Double = 0d
  var timePosLength: Int = 0

  override def toString(): String = {
    // return s"$lats$table$lons$symbol"
    return s"$lats,$lons $table$symbol"
  }

  def setPos(la: String, lo: String, ta: String, sm: String) {
    table = ta
    symbol = sm
    lats = la
    lons = lo
  }

  def parsePosition(payload: String) = {
    // val (slat, slon) = AprsisStation.lastLocation()
    /* Skipping match/case idiom for Some/None, so
       that only one run of the regex is performed. */
    var trial = (our.latLonRegex findFirstMatchIn payload)
    if (trial != None) {
      val m = trial.get
      setPos(m.group("lat"), m.group("lon"), m.group("table"), m.group("symbol"))
      timePosLength = m.end
    }
    trial = our.compressedLatLonRegex findFirstMatchIn payload
    if (trial != None) {
      val m  = trial.get // (time, ta, lar, lor, sm)
      latf = our.expandLat( m.group(3))
      lonf = our.expandLon(m.group(4))
      val la = our.stringLat(latf)
      val lo = our.stringLon(lonf)
      val ta = m.group("table") match {
        case "a" => "0"
        case "b" => "1"
        case "c" => "2"
        case "d" => "3"
        case "e" => "4"
        case "f" => "5"
        case "g" => "6"
        case "h" => "7"
        case "i" => "8"
        case "j" => "9"
        case x => x
      }
      setPos(la, lo, ta, m.group("symbol"))
      timePosLength = m.end - 3 // Course/speed match correction
    }
  }
  def position(): Tuple2[Double, Double] = {
    if (latf == 0d && lats != "0000.00N") latf = our.dddmmmmmToDouble(lats)
    if (lonf == 0d && lons != "00000.00W") lonf = our.dddmmmmmToDouble(lons)
    return (latf, lonf)
  }

}
object AprsPosition {
  val latLonRegex = new Regex( // spec. pages 32 to 35
    """^((?:\d{6}[zh])|)(\d{4}\.\d\d[NS])([\\\/0-9A-Z])(\d{5}\.\d\d[EW])(.)""",
    "date", "lat", "table", "lon", "symbol")
  val compressedLatLonRegex = new Regex( // spec pp. 33-41
    """^((?:\d{6}[zh])|)([\\\/A-Za-j])([!-|]{4})([!-|]{4})(.)([!-\|][!-|])[!-|]""",
    "date", "table", "zlat", "zlon", "symbol", "courseSpeed")
  val parseDegMin = new Regex("""(1?\d{2})([0-6]\d\.\d{2})([NSEWnsew])""", "deg", "min", "hemi")

  def dddmmmmmToDouble(degs: String): Double = {
    val dmm = parseDegMin findFirstMatchIn degs
    if (dmm == None) {
      try {
        return degs.toDouble
      } catch {
        case e: Exception => return 0f
      }
    }
    val m = dmm.get
    val sign = m.group("hemi") match {
      case "N" | "E" | "n" | "e" => 1
      case "W" | "S" | "s" | "w" => -1
    }
    return sign * (m.group("deg").toDouble + (m.group("min").toDouble /60))
  }

  def apply(payload: String) = {
    val d = new AprsPosition();
    d.parsePosition(payload)
    d
  }

  /**
    Lat = 90 - ((y1-33) x 91^3 + (y2-33) x 91^2 + (y 3-33) x 91 + y4-33) / 380926
    Long = -180 + ((x1-33) x 913 + (x2-33) x 912 + (x3-33) x 91 + x4-33) / 190463
    For example, if the compressed value of the longitude is <*e7 (as computed
    above), the calculation becomes:
    Long = -180 + (27 x 913 + 9 x 912 + 68 x 91 + 22) / 190463
         = -180 + (20346417 + 74529 + 6188 + 22) / 190463
         = -180 + 107.25
         = -72.75 degrees
    */

  def expandLon(llc: String): Double = {
    return if(llc.size < 1) 0d else ((base91decode(llc)/190463.0) - 180).toDouble
  }
  def expandLat(llc: String): Double = {
    return if (llc.size < 1) 0d else (90.0 - (base91decode(llc)/380926.0)).toDouble
  }

  def stringLon(doubleLon: Double): String = {
    var h = "E"
    var l: Double = 0.0
    if (doubleLon < 0) {h = "W"; l = -doubleLon}; else l = doubleLon;
    val ld = l.toInt
    val lmin = (l - ld) * 60
    return f"$ld%03d$lmin%05.2f$h%s"
  }

  def stringLat(doubleLat: Double): String = {
    var h = "N"
    var l: Double = 0.0
    if (doubleLat < 0) {h = "S"; l = -doubleLat}; else l = doubleLat;
    val ld = l.toInt
    val lmin = (l - ld) * 60
    return f"$ld%02d$lmin%05.2f$h%s"
  }

}
