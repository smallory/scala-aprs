package net.hcoop.smallory.scalaprs

import org.scalatest.{FunSpec}

class aprsPacketTest extends FunSpec {
    def seedTime() {
      // Set the time the AprsisPacket class object
      AprsisPacket("# javAPRSSrvr 4.1.0b05 22 Nov 2016 14:23:39 GMT WE7U-F2 14580")
      return
    }
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
      it("should use javAPRSSrvr server comments to set date stream time") {
        val a = AprsisPacket("# javAPRSSrvr 4.1.0b05 21 Nov 2016 03:18:32 GMT WE7U-F2 14580") 
        assert(AprsisPacket.year == "2016")
        assert(AprsisPacket.month == "Nov")
        assert(AprsisPacket.day == "21")
        assert(AprsisPacket.hour == "03")
        assert(AprsisPacket.minute == "18")
        assert(a.comment == true)
      }
      it("should use aprsc server comments to set date stream time") {
        val b = AprsisPacket("# aprsc 2.0.20-g6a459af 18 Nov 2016 17:19:49 GMT T2USANE 107.170.42.65:14580")
        assert(AprsisPacket.year == "2016")
        assert(AprsisPacket.month == "Nov")
        assert(AprsisPacket.day == "18")
        assert(AprsisPacket.hour == "17")
        assert(AprsisPacket.minute == "19")
        assert(b.comment == true)
      }
      it("should use time in more recently read server comment as date stream time") {
        val a = AprsisPacket("# javAPRSSrvr 4.1.0b05 21 Nov 2016 03:18:32 GMT WE7U-F2 14580") 
        val b = AprsisPacket("# aprsc 2.0.20-g6a459af 18 Nov 2016 17:19:49 GMT T2USANE 107.170.42.65:14580")
        assert(AprsisPacket.year == "2016")
        assert(AprsisPacket.month == "Nov")
        assert(AprsisPacket.day == "18")
        assert(AprsisPacket.hour == "17")
        assert(AprsisPacket.minute == "19")
        assert(b.comment == true)
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
      it("should provide flat records of one measurement each") {
        seedTime()
        val ap = AprsisPacket("WD4IXD>APRS,TCPIP*,qAC,T2USANW:@221520z3930.75N/10500.95W_308/003g007t033r002p010P010b10158h92L026.WD 31")
        // first check that the expected things are there
        assert(ap != null)
        assert(ap.source != null)
        assert(ap.destination != null)
        assert(ap.payload != null)
        assert(ap.date != null)
        assert(ap.position != null)
        assert(ap.weather != null)
        // check that intermediate values are correctly poopulated
        assert(ap.comment === false)
        assert(ap.payload === "221520z3930.75N/10500.95W_308/003g007t033r002p010P010b10158h92L026.WD 31")
        assert(ap.position.lats === "3930.75N")
        assert(ap.position.lons === "10500.95W")
        assert(ap.position.table === "/")
        assert(ap.position.symbol === "_")
        assert(ap.date.theDate === "2016 Nov 22, 15:20")
        // do we have the data of interest?
        assert(ap.weather.wx.size === 10)
        // Do the things!
        val flat = ap.getWxObservations()
        assert(flat != null)
        assert(flat != None)
        assert(flat.size === 10)
      }
    }
  }
}

