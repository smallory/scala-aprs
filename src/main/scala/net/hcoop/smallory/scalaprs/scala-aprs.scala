/*
 * Copyright 2016 Sean Mallory
 * 
 * All rights reserved until license chosen.
 * 
 */

package net.hcoop.smallory.scalaprs

import org.apache.spark.SparkContext
import org.apache.spark.sql.SparkSession

import scala.collection.mutable.{Map, ArrayBuffer}

object scalaprs {
  def main(args: Array[String]) {
    val sps = SparkSession
      .builder()
      .appName("scala-aprs")
      .getOrCreate()

    val settings = getSettings(sps.sparkContext, args)

    settings("action") match {
      case "help" => helpInfo()
      case "ingest" => aprsIngest(sps, settings)
      case "serve" => alertsServer(sps, settings)
    }
  }

  /**
    Get settings from arguments and settings file
    */
  def getSettings(sc: SparkContext, args: Array[String]):
      Map[String, String] = {
    val tgt = args.size match {
      case 0 => "help"
      case _ => args(0).toLowerCase
    }
    tgt match {
          case "ingest" => "ingest"
          case "model" | "process" => "model"
          case _ => "help"
    }
    val settingsFile = (
      if (args.size > 1) args(1)
      else "scalaprs.config"
    )
    val settingArray = sc.textFile(settingsFile).collect()
    val mm = mapFromStrings(settingArray)
    mm += ("action" -> tgt)
    mm += ("settingsFile" -> settingsFile)
    return mm
  }

  /**
    Send out requested notifications from alerts
    */
  def helpInfo() = {
    println("""spark-submit <path to scala-aprs.jar> <ingest|serve|help> [configuration file]""")
  }

}
