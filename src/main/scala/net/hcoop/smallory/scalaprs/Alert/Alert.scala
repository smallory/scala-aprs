/**
  Alerts define the conditions that lead to a message to a user being created
  */

package net.hcoop.smallory.scalaprs.alerts

trait Alert {
  def comparison: String // = ">"
  def compareTo: String // ""|"max","min"
  def limit: Double // = Double.NaN
  def message: String // = ""
  def models: Vector[String]
  def value(models: Map[String, Model]): Double

  def apply(models: Map[String, Model]): Option[String] = {
    val epsilon = 0.00001d
    val av = value(models)
    if (av != Double.NaN) comparison match {
      case ">"  | "gt" =>
        if (av > limit) Some(message) else None
      case ">=" | "ge" | "=>" =>
        if (av >= limit) Some(message) else None
      case "="  | "eq" | "==" =>
        if (limit + epsilon > av && av > limit - epsilon)
          Some(message) else None
      case "!=" | "ne" | "<>" =>
        if (av != limit) Some(message) else None
      case "<=" | "le" | "=<" =>
        if (av <= limit) Some(message) else None
      case "<"  | "lt" =>
        if (av < limit) Some(message) else None
    } else None
  }
}

object Alert {
  def apply(lert: String):Alert = {
    val parts: Array[String] = lert.split(" ")
    return parts(0).toLowerCase match {
      case "t" | "temperature" => TemperatureAlert(parts)
      case "X" | "radiation" => RadiationAlert(parts)
      case "g"|"c"|"s"|"t"|"r"|"p"|"P"|"h"|"b"|"L"|"l"|"s"|"#"|"F"|"f"|"V"|"Z"|"X" =>
        GeneralAlert(parts)
    }
  }

  /*
    TODO:
    This is here as a note on how to improve this part, but
    the supporting *.apply(x: MatchObject) are not available
    yet. Thus this is commented out as a suggection until it
    won't break the build.
    */
  // def applyRegex(lert: String): Option[Alert] = {
  //   val alertRegex =
  //     """([a-zA-Z0-9]+) *(min|max|low|high|)\w*([=><]*)\w*(\w[a-zA-Z0-9.-]|)""".r
  //   return (alertRegex findFirstIn lert) match {
  //     case None => None
  //     case Some(mch) =>
  //       mch(0).toLowerCase match {
  //         case "t" | "temperature" => TemperatureAlert(mch)
  //         case "X" | "radiation" => RadiationAlert(mch)
  //         case "h"|"l"|"s"|"c"|"p"|"P" => GeneralAlert(mch)
  //       }
  //   }
  //}
}
