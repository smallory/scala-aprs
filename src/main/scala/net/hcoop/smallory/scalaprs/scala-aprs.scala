/*
 * Copyright 2016 Sean Mallory
 * 
 * All rights reserved until license chosen.
 * 
 */

package net.hcoop.smallory.scalaprs

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
//import org.apache.spark.streaming._

object scala-aprs {
  def main(args: Array[String]) {
    val sc = new SparkContext()
    val inFile = ( if (args.size > 0)
      args(0)
    else "src/test/scala/net/hcoop/smallory/scala-aprs/test.dat" )
    // Using a stub input for development, after authentication is built,
    // switch to streaming socket from aprs-is server.
    val distFile = sc.textFile(inFile)

    distFile
      .map(rec => AprsisPacket(rec) )
      .filter(rec => !rec.comment)
      .filter(pkt => pkt.position.symbol == "_")
      .flatMap(r => r.getWxObservations())
      .take(100)
      .map(println)

  }
}
