package net.hcoop.smallory.freezewarn

import org.scalatest.{FunSpec}

class aprsPacketTest extends FunSpec {
  describe("the aprsPackets") {
    describe("when being created") {
      it("should identify but not load comments") {
        val a = AprsisPacket("# javAPRSSrvr 4.1.0b05 21 Nov 2016 03:18:32 GMT WE7U-F2 14580")
        assert(a.payload == null)
        assert(a.comment == true)
      }
      it("should treat blank lines as empty comments") {
        val a = AprsisPacket("")
        assert(a.payload == null)
        assert(a.comment == true)
      }
      it("should use server comments to set date stream time") {
        val a = AprsisPacket("# javAPRSSrvr 4.1.0b05 21 Nov 2016 03:18:32 GMT WE7U-F2 14580") 
        assert(AprsisPacket.year == "2016")
        assert(AprsisPacket.month == "Nov")
        assert(AprsisPacket.day == "21")
        assert(AprsisPacket.hour == "03")
        assert(AprsisPacket.minute == "18")
        assert(a.comment == true)
      }
      it("should load weather data when present in 'H'azard objects") {
        val ht = AprsisPacket("N0LNE>APRS,TCPIP*,qAC,EIGHTH:@181545z3935.04N/10510.26WH150/005g008t026h  X123")
        assert(ht != null)
        assert(ht.comment === false)
        assert(ht.payload === "181545z3935.04N/10510.26WH150/005g008t026h  X123")
        assert(ht.weather != null)
        assert(ht.weather.wx.size === 5)
        assert(ht.weather.get("X").isDefined)
        assert(ht.weather.get("X").get === 12000)
        assert(ht.weather.get("c").get === 150)
        assert(ht.weather.get("s").get === 5)
        assert(ht.weather.get("g").get === 8)
        assert(ht.weather.get("t").get === 26)
        assert(!ht.weather.get("h").isDefined)
      }
    }
  }
}

