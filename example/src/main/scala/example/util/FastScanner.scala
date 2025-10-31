package example.util

import java.io.InputStream

final class FastScanner(private val in: InputStream) {
  private val buffer = new Array[Byte](1 << 16)
  private var ptr = 0
  private var len = 0

  private val EndOfStream = -1
  private val Space = 0x20

  private def readByte(): Int = {
    if (ptr >= len) {
      len = in.read(buffer)
      ptr = 0
      if (len <= 0) { return EndOfStream }
    }
    val res = buffer(ptr) & 0xff
    ptr += 1
    res
  }

  private def skipWhitespace(): Int = {
    var b = readByte()
    while (b != EndOfStream && b <= Space) {
      b = readByte()
    }
    b
  }

  def next(): String = {
    val sb = new java.lang.StringBuilder
    var b = skipWhitespace()
    while (b != EndOfStream && b > Space) {
      sb.append(b.toChar)
      b = readByte()
    }
    sb.toString
  }

  def nextInt(): Int = {
    var b = skipWhitespace()
    var sign = 1
    if (b == '-') {
      sign = -1
      b = readByte()
    }
    var num = 0
    while (b != EndOfStream && b > Space) {
      num = num * 10 + (b - '0')
      b = readByte()
    }
    sign * num
  }

  def nextLong(): Long = {
    var b = skipWhitespace()
    var sign = 1L
    if (b == '-') {
      sign = -1L
      b = readByte()
    }
    var num = 0L
    while (b != EndOfStream && b > Space) {
      num = num * 10 + (b - '0')
      b = readByte()
    }
    sign * num
  }

  def nextLine(): String = {
    val sb = new java.lang.StringBuilder
    var b = readByte()
    while (b != EndOfStream && b != '\n') {
      if (b != '\r') {
        sb.append(b.toChar)
      }
      b = readByte()
    }
    sb.toString
  }
}
