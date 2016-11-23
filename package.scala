/**
  Package utilities
  */
package net.hcoop.smallory

package object freezewarn {
  def base91decode(str: String): Long = {
    var l: Long = 0
    for (c <- str) {
      l *= 91
      l += c - 33
    }
    return l
  }

  def base91encode(num: Long): String = {
    var s: String = ""
    var n: Long = num
    while (n > 0) {
      s = ((n % 91) + 33).toChar + s
      n = n/91
    }
    return s
  }

}
