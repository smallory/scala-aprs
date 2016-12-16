/*
 * Copyright 2016 Sean Mallory
 * 
 * All rights reserved until license chosen.
 * 
 */

package net.hcoop.smallory.scalaprs

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.sql.{SparkSession, DataFrame, Dataset}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SQLContext
import org.apache.spark.streaming.{StreamingContext, Minutes}

import scala.collection.mutable.{Map, ArrayBuffer}

object aprsIngest {
  def apply(sps: SparkSession, settings: Map[String, String]) {
    val spc = sps.sparkContext
    val sqc = sps.sqlContext
    val ssc = new StreamingContext(spc, Minutes(2))

    val inFile = settings.getOrElse("dataSource",
      "src/test/scala/net/hcoop/smallory/scalaprs/test.dat")
    val dataStore = settings.getOrElse("dataStore",
      "dev.wx.store")
    parseData(spc.textFile(inFile), sqc)
      .write.json(dataStore)
  }
  /**
    Data loading process

    In operation, will take TCP stream and transform to data store updates.
    */
  def parseData(source: RDD[String], sqc: SQLContext ):
      Dataset[WxObservation] = {
    // Using a stub input for development, after authentication is built,
    // switch to streaming socket from aprs-is server.
    import sqc.implicits._
    source
      .map(rec => AprsisPacket(rec) )
      .filter(rec => !rec.comment)
      .filter(pkt => pkt.position.symbol == "_")
      .flatMap(r => r.getWxObservations())
      .toDF().as[WxObservation]
  }
}
