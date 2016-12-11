/*
 * Copyright 2016 Sean Mallory
 * 
 * All rights reserved until license chosen.
 * 
 */

package net.hcoop.smallory.scalaprs

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SQLContext
//import org.apache.spark.streaming._

import scala.collection.mutable.{Map, ArrayBuffer}

object scalaprs {
  def main(args: Array[String]) {
    val spc = new SparkContext()
    val sqc = new org.apache.spark.sql.SQLContext(spc)

    //-----------------------------------
    // Get the configuartion/settings map
    val settings = getSettings(spc, args)
    //-----------------------------------

    //-----------------------------------
    // Start the data load stream first, to miss least data
    val inFile = settings.getOrElse("dataSource",
      "src/test/scala/net/hcoop/smallory/scalaprs/test.dat")
    val dataStore = settings.getOrElse("dataStore",
      "dev.wx.store")
    loadData(spc.textFile(inFile), dataStore, sqc)
    //-----------------------------------

    //-----------------------------------
    // Model / output operational loop
    // Once out of once-and-done dev testing, should
    // migrate to a streaming sink or similar.
    val userFile = settings.getOrElse("users", "users_test.dat")
    for (i <- List(1)) {
      //-----------------------------------
      // Read the user file for contact info and alert definitions
      var uu = findUsers(userFile)

      //-----------------------------------
      // Get an active alert list
      var aa = updateAlerts(dataStore, uu)

      //-----------------------------------
      // Do the active alert notification
      notifyUsers(aa)

    }
    // End model / output loop
    //-----------------------------------
  }

  /**
    Get settings from arguments and settings file
    */
  def getSettings(sc: SparkContext, args: Array[String]):
      Map[String, String] = {
    val settingsFile = (
      if (args.size > 0) args(0)
      else "scalaprs.config"
    )
    val settingArray = sc.textFile(settingsFile).collect()
    mapFromStrings(settingArray)
  }

  /**
    Data loading process

    In operation, will take TCP stream and transform to data store updates.
    */
  def loadData(source: RDD[String], store: String, sqc: SQLContext) = {
    // Using a stub input for development, after authentication is built,
    // switch to streaming socket from aprs-is server.
    import sqc.implicits._
    val df = source
      .map(rec => AprsisPacket(rec) )
      .filter(rec => !rec.comment)
      .filter(pkt => pkt.position.symbol == "_")
      .flatMap(r => r.getWxObservations())
      // .takeSample(false, 300)
      // .map(r => logNote(r.toString))
      .toDF()
    //.saveAsObjectFile("sample.wx.dat")

    df.as[WxObservation]
      .write.parquet(store)
  }

  /**
    Read the user configuration file and prepare for processing
    */
  def findUsers(userFile: String): ArrayBuffer[User] = {
    val uu: ArrayBuffer[User] = ArrayBuffer()
    uu
  }

  /**
    Run the models and test the alerts that were specified in the user file
    */
  def updateAlerts(store: String, users: Iterable[User]): Array[Alert] = {
    val aa: Array[Alert] = Array()
    aa
  }

  /**
    Send out requested notifications from alerts
    */
  def notifyUsers(aa: Array[Alert]) = {
  }

}
