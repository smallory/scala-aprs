/*
 * Copyright 2016 Sean Mallory
 * 
 * All rights reserved until license chosen.
 * 
 */

package net.hcoop.smallory.scalaprs

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import scala.collection.mutable.Map
//import org.apache.spark.streaming._

object scalaprs {
  def main(args: Array[String]) {
    val sc = new SparkContext()

    // Get settings from settings file
    val settingsFile = (
      if (args.size > 0)
        args(0)
      else "scalaprs.config" )
    val settingArray = sc.textFile(settingsFile).collect()
    val settings = readMapFromStrings(settingArray)

    // Set settings found in settings file
    val userFile = settings.getOrElse("users", "users_test.dat")
    val inFile = settings.getOrElse("testData",
      "src/test/scala/net/hcoop/smallory/scalaprs/test.dat")

    // Using a stub input for development, after authentication is built,
    // switch to streaming socket from aprs-is server.
    val distFile = sc.textFile(inFile)

    distFile
      .map(rec => AprsisPacket(rec) )
      .filter(rec => !rec.comment)
      .filter(pkt => pkt.position.symbol == "_")
      .flatMap(r => r.getWxObservations())
      .takeSample(false, 300)
      .map(r => logNote(r.toString))
  }

  def readMapFromStrings(arFile: Iterable[String]): Map[String, String] = {
    var rtrn: Map[String, String] = Map()
    rtrn ++= arFile map { (entry: String) =>
      val field = entry.split("=")
      if (field.size > 1) Some(field(0) -> field(1))
      else None
    } collect ({case Some((x:String, y:String)) => (x -> y)})
    return rtrn
  }
}
