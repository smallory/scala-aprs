/**
  Class for operations on the times of APRS packets

  Depends on having the data-stream time available from AprsisPacket

  */

package net.hcoop.smallory.scalaprs

import java.time.ZonedDateTime

case class WxObservation (
  lat: Float,
  lon: Float,
  time: ZonedDateTime,
  feature: String,
  value: Float,
  unit: String
)
