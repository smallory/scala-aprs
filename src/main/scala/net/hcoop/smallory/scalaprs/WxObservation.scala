/**
  Weather observation class
  */

package net.hcoop.smallory.scalaprs

case class WxObservation (
  lat: Double,
  lon: Double,
  time: Long, // Second from Epoch
  id: String,
  value: Double,
  unit: String
)

/*
 Tools for intechanging Zoneddatetime and Long(seconds)

import java.time.Instant
val t: ZonedDateTime = Instant.ofEpochSecond(time).atZone(utc)

val s: Long = (ZonedDateTime instance).toInstant.getEpochSecond()

 */
