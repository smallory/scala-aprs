package net.hcoop.smallory.freezewarn

import org.scalatest.{FunSpec}
import org.scalatest.Matchers._

class AprsDateTest extends FunSpec {
  describe("in aprsDate object") {
    def seedTime() {
      // Set the time the AprsisPacket class object
      AprsisPacket("# javAPRSSrvr 4.1.0b05 22 Nov 2016 14:23:39 GMT WE7U-F2 14580")
      return
    }
    it("should be based on AprsisPacket stread date when absent") {
      seedTime()
      // K6DHN-9>APT314,W0UPS-5,WIDE1*,WIDE2-1,qAR,W0ARP:>TT3, K6DHN@COMCAST.NET
      val a = AprsDate("TT3, K6DHN@COMCAST.NET")
      assert(a.theDate === "2016 Nov 22, 14:23")
    }
    it("should parse ddhhmmz timestamps") {
      // N0LNE>APRS,TCPIP*,qAC,FIFTH:@221420z3935.04N/10510.26W_028/008g013t036r000p003P003b10110h78.WD 233
      val a = AprsDate("221420z3935.04N/10510.26W_028/008g013t036r000p003P003b10110h78.WD 233")
      assert(a.theDate === "2016 Nov 22, 14:20")
    }

    it("should be able to digest odd characters in Mic-E") {
      // K0LAI-9>SY4PTR,WIDE1-1,WIDE2-1,qAR,W0ARP:`qal v/]\"GV}449.350MHzÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ=
      val a = AprsDate("qal v/]\"GV}449.350MHzÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ=")
      assert(a.theDate === "2016 Nov 22, 14:23")
    }
    it("should use datastream timee when the timestamp is missing") {
      // N4ATA-7>APBPQ1,TCPIP*,qAC,EIGHTH:!3940.96N/10503.04WB BPQ32 Igate V 6.0.9.1
      val a = AprsDate("3940.96N/10503.04WB BPQ32 Igate V 6.0.9.1")
      assert(a.theDate === "2016 Nov 22, 14:23")
    }
    it("does not recognise 'English' times.") {
      // K0MTN-10>APWW10,TCPIP*,qAC,T2OSAKA:>DM79koI&DX: KC0D 29.9mi 125° 14:20 3922.20N 10440.76W
      val a = AprsDate("DM79koI&DX: KC0D 29.9mi 125° 14:20 3922.20N 10440.76W")
      assert(a.theDate === "2016 Nov 22, 14:23")
    }
      // K0XK>APDR13,TCPIP*,qAC,T2SWEDEN:=3936.47NZ10448.47W@313/000/A=005620 http://aprsdroid.org/
    // K6DHN-9>APT314,WIDE1-1,WIDE2-1,qAR,W0ARP:!3933.39N/10453.34WV115/000/A=005960!wSj!
    // K6DHN-9>APT314,WIDE1-1,WIDE2-1,qAR,W0ARP:>TT3, K6DHN@COMCAST.NET
    // W0JRL-15>APRS,TCPIP*,qAC,T2MAZURY:=3945.31N/10459.20WxPHG4270/447.175 MHz -5.00 MHz CTCSS 100.0 Hz. (AllStar Link Node 29997/EchoLink Node 201712)
    // N4ATA-7>APBPQ1,TCPIP*,qAC,EIGHTH:>BPQ32 DVRCO:N4ATA-7 Node-BBS-RMS Denver, CO
    // METAR-2>APFMET,TCPIP*,qAC,WE7U-F2:;MTRKBJC  *221415z3955.00N/10507.00W_350/015g...t035h93b10122 CO_Broomfield/Jeffco
    // N1GEP-1>S9TPSV,WIDE1-1,WIDE2-1,qAR,KC7SBS:`pIwl Wp/'"G5}!SN! 449.625 pl141.3|"+%J'e|!w;*!|3
    // # javAPRSSrvr 4.1.0b05 22 Nov 2016 14:23:39 GMT WE7U-F2 14580
    // AE5VQ>APRS,TCPIP*,qAC,T2ONTARIO:@221422z3936.00N/10521.23W_090/002g005t032r000p014P014b10201h99eMH50
  }
}
