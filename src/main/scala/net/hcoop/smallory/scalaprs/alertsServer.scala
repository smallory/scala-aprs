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

object alertsServer {
  def apply(sps: SparkSession, settings: Map[String, String]) {
    val spc = sps.sparkContext
    val sqc = sps.sqlContext

    val dataStore = settings.getOrElse("dataStore",
      "dev.wx.store")

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
