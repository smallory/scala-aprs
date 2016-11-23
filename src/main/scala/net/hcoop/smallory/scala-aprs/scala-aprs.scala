/*
 * Copyright 2016 Sean Mallory
 * 
 * All rights reserved until license chosen.
 * 
 */

package net.hcoop.smallory.freezewarn

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
//import org.apache.spark.streaming._

object freezewarn {
  def main(args: Array[String]) {
    val sc = new SparkContext()
    // Using a stub input for development, after authentication is built,
    // switch to streaming socket from aprs-is server.
    val distFile = sc.textFile("src/test/scala/net/hcoop/smallory/scala-aprs/test.dat")

    distFile
        .map(rec => AprsisPacket(rec) )
        .filter(rec => !rec.comment)
        .filter(pkt => pkt.position.symbol =="_")
        .map(pkt => pkt.fmt() )
        .take(100)
        .map(println)

  }
}
