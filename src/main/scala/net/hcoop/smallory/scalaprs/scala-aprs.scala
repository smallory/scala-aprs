/*
 * Copyright 2016 Sean Mallory
 * 
 * All rights reserved until license chosen.
 * 
 */

package net.hcoop.smallory.scalaprs

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.sql.SparkSession
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SQLContext

//import org.apache.spark.streaming._

import scala.collection.mutable.{Map, ArrayBuffer}

object scalaprs {
  def main(args: Array[String]) {
    val spc = new SparkContext()
    val sps = SparkSession
      .builder()
      .appName("scala-aprs")
      .config(spc.getConf)
      .getOrCreate()
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
    loadData(spc.textFile(inFile), dataStore, sqc, sps)
    //-----------------------------------

    // Read the user file for contact info and alert definitions
    val userFile = settings.getOrElse("users", "users_test.dat")
    var uu = findUsers( spc.textFile(userFile) )

    //-----------------------------------
    // Model / output operational loop
    // After the once-and-done dev testing phase is finished,
    // should migrate to a streaming sink or similar.
    import sps.implicits._
    //import sqc.implicits._
    val dataDF = sps.read.format("json").load(dataStore).as[WxObservation]
    dataDF.show()
    // map over Dataframe generates errors due to lack of "Row" Extractor
    // so convert to RDD before map. (Spark 2.0 bug)
    dataDF.rdd.map(row => {
        uu.map(u => {
          u.addObservation(row)
          u.checkAlerts().foreach(logInfo)
        })
    })

    for (i <- List(1)) {
      // Get an active alert list
      var aa = updateAlerts(dataStore, uu)
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
  def loadData(source: RDD[String], store: String,
    sqc: SQLContext, sps: SparkSession ) = {
    // Using a stub input for development, after authentication is built,
    // switch to streaming socket from aprs-is server.
    // import sps.implicits._  // breaks toDF() ???
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
      .write.json(store)
  }

  /**
    Read the user configuration file and prepare for processing
    */
  def findUsers(userFile: RDD[String]): ArrayBuffer[User] = {
    val uu: ArrayBuffer[User] = ArrayBuffer()
    // User() returns Option[User], flatMap unrolls this to RDD[User]
    uu ++= userFile
      .flatMap(userString => User(userString) )
      .collect
    logDebug("Counted "+uu.size+" valid user strings")
    uu
  }

  /**
    Run the models and test the alerts that were specified in the user file
    */
  def updateAlerts(store: String, users: Iterable[User]): Array[alerts.Alert] = {
    val aa: Array[alerts.Alert] = Array()
    aa
  }

  /**
    Send out requested notifications from alerts
    */
  def notifyUsers(aa: Array[alerts.Alert]) = {
    // stub
  }

}
