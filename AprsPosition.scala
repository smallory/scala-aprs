/**
  Class for operations on the positions of APRS packets
  */

package net.hcoop.smallory.freezewarn

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
  import net.hcoop.smallory.freezewarn.{AprsPosition => our}
  var lats: String = "0000.00N"
  var lons: String = "0000.00W"
  var error: Int = 6
  var maidenhead: String = null
  var table: String = "\\"
  var symbol: String = "."
  var latf: Double = 0f
  var lonf: Double = 0f
  var timePosLength: Int = 0

  override def toString(): String = {
    // return s"$lats$table$lons$symbol"
    return s"$lats,$lons $table$symbol"
  }

  def setPos(la: String, lo: String, ta: String, sm: String) {
    table = ta
    symbol = sm
    lats = la
    lons = la
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
      val m  = trial.get // (ta, lar, lor, sm)
      latf = our.expandLat( m.group(2))
      lonf = our.expandLon(m.group(3))
      val la = our.stringLat(latf)
      val lo = our.stringLon(lonf)
      setPos(la, lo, m.group(1), m.group(4))
      // TODO: numbers are still letters in table identifier
      timePosLength = m.end
    }
  }
}
object AprsPosition {
  val latLonRegex = new Regex( // spec. pages 32 to 35
    """(\d\d\d\d\.\d\d[NS])([\\\/0-9A-Z])(\d\d\d\d\d\.\d\d[EW])(.)""",
    "lat", "table", "lon", "symbol")
  val compressedLatLonRegex = new Regex( // spec pp. 33-41
    """([\\\/A-Za-j])(\d\d\d\d)(\d\d\d\d)(.)(..)T""",
    "table", "zlat", "zlon", "symbol", "courseSpeed")

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
    return if(llc.size < 1) 0 else ((base91decode(llc)/190463.0) - 180)
  }
  def expandLat(llc: String): Double = {
    return if (llc.size < 1) 0 else (90.0 - (base91decode(llc)/380926.0))
  }

  def doubleLL(lons: String): Double = {
    val i = lons.length - 1
    val hemi = lons drop i take 1
    val deg = (lons take 2).toInt + ((lons drop 2 take i-2).toDouble / 60)
    if (hemi == "N" || hemi == "E") return deg
    else if (hemi == "W" || hemi == "S") return -deg
    else return 0.0d
  }

  def stringLon(doubleLon: Double): String = {
    var h = "E"
    var l: Double = 0.0
    if (doubleLon < 0) {h = "W"; l = -doubleLon}; else l = doubleLon;
    val ld = l.toInt
    val lmin = (l - ld) * 60
    return f"$ld%03d$lmin%02.2f$h%s"
  }

  def stringLat(doubleLat: Double): String = {
    var h = "N"
    var l: Double = 0.0
    if (doubleLat < 0) {h = "S"; l = -doubleLat}; else l = doubleLat;
    val ld = l.toInt
    val lmin = (l - ld) * 60
    return f"$ld%02d$lmin%02.2f$h%s"
  }

}