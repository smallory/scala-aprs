/**
  Weather observation class
  */

package net.hcoop.smallory.scalaprs

import java.time.ZonedDateTime

case class WxObservation (
  lat: Float,
  lon: Float,
  time: Long, // Second from Epoch
  feature: String,
  value: Float,
  unit: String
)

/*
 Tools for intechanging Zoneddatetime and Long(seconds)

import java.time.Instant
val t: ZonedDateTime = Instant.ofEpochSecond(time).atZone(utc)

val s: Long = (ZonedDateTime instance).toInstant.getEpochSecond()

 */
