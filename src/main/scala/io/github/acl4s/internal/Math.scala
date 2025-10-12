package io.github.acl4s.internal

import scala.math.BigInt

/**
 * @param x
 * @param m `1 <= m`
 * @return x mod m
 */
private[acl4s] def safeMod(x: Long, m: Long): Long = {
  var v = x % m
  if (v < 0L) { v += m }
  v
}

val ULongMaxValue = BigInt(2).pow(64) - 1

/**
 * Fast modular by barrett reduction
 * Reference: https://en.wikipedia.org/wiki/Barrett_reduction
 * NOTE: reconsider after Ice Lake
 */
final private[acl4s] case class Barrett(m: Int) {
  val im = ULongMaxValue / m + 1L

  /**
   * @param a `0 <= a < m`
   * @param b `0 <= b < m`
   * @return `a * b % m`
   */
  def mul(a: Int, b: Int): Int = {
    // [1] m = 1
    // a = b = im = 0, so okay

    // [2] m >= 2
    // im = ceil(2^64 / m)
    // -> im * m = 2^64 + r (0 <= r < m)
    // let z = a*b = c*m + d (0 <= c, d < m)
    // a*b * im = (c*m + d) * im = c*(im*m) + d*im = c*2^64 + c*r + d*im
    // c*r + d*im < m * m + m * im < m * m + 2^64 + m <= 2^64 + m * (m + 1) < 2^64 * 2
    // ((ab * im) >> 64) == c or c + 1
    val z = a.toLong * b.toLong
    val x = (im * z) >> 64
    val y = (x * m).toLong
    (z - y + (if (z < y) { m.toLong }
              else { 0L })).toInt
  }
}

/**
 * @param x
 * @param n `0 <= n`
 * @param m `1 <= m`
 * @return `(x ** n) % m`
 */
private[internal] def powMod(x: Long, n: Long, m: Int): Long = {
  if (m == 1) { return 0L }

  val _m = m.toLong
  var _n = n
  var r = 1L
  var y = safeMod(x, _m)
  while (_n > 0L) {
    if ((_n & 1) != 0) { r = (r * y) % _m }
    y = (y * y) % _m
    _n >>= 1L
  }
  r
}

// strong probable-prime with base b, If v < 2.pow(32)
val bSPRP = Array(2L, 7L, 61L)

/**
 * Reference:
 * M. Forisek and J. Jancina,
 * Fast Primality Testing for Integers That Fit into a Machine Word
 *
 * @param v `0 <= n`
 * @return
 */
@annotation.nowarn("msg=Non local returns are no longer supported;")
private[acl4s] def isPrime(v: Int): Boolean = {
  v match {
    case _ if v <= 1     => return false
    case 2 | 7 | 61      => return true
    case _ if v % 2 == 0 => return false
    case _               =>
  }
  val n = v.toLong
  val n_1 = n - 1
  val d = {
    var d = n_1
    while (d % 2 == 0) { d /= 2 }
    d
  }
  for (a <- bSPRP) {
    var t = d
    var y = powMod(a, t, n.toInt)
    while (t != n_1 && y != 1L && y != n_1) {
      y = y * y % n
      t <<= 1L
    }
    if (y != n_1 && t % 2 == 0) {
      return false
    }
  }
  true
}

/**
 * @param a
 * @param b `1 <= b`
 * @return (g, x) s.t. g = gcd(a, b), xa = g (mod b), 0 <= x < b/g
 */
private[acl4s] def invGcd(a: Long, b: Long): LPair = {
  val _a = safeMod(a, b);
  if (_a == 0L) {
    return LPair(b, 0L)
  }

  // Contracts:
  // [1] s - m0 * a = 0 (mod b)
  // [2] t - m1 * a = 0 (mod b)
  // [3] s * |m1| + t * |m0| <= b
  var s = b
  var t = _a
  var m0 = 0L
  var m1 = 1L

  while (t > 0L) {
    val u = s / t
    s -= t * u
    m0 -= m1 * u // |m1 * u| <= |m1| * s <= b

    // [3]:
    // (s - t * u) * |m1| + t * |m0 - m1 * u|
    // <= s * |m1| - t * u * |m1| + t * (|m0| + |m1| * u)
    // = s * |m1| + t * |m0| <= b

    var tmp = s
    s = t
    t = tmp
    tmp = m0
    m0 = m1
    m1 = tmp
  }
  // by [3]: |m0| <= b/g
  // by g != b: |m0| < b/g
  if (m0 < 0L) { m0 += b / s }
  LPair(s, m0)
}

/**
 * @param n `n < 2^32`
 * @param m `1 <= m < 2^32`
 * @param a
 * @param b
 * @return `sum_{i=0}^{n-1} floor((ai + b) / m) (mod 2^64)`
 */
private[acl4s] def floorSumUnsigned(n: Long, m: Long, a: Long, b: Long): Long = {
  var _n = n
  var _m = m
  var _a = a
  var _b = b
  var ans = 0L

  while (true) {
    if (_a >= _m) {
      ans += _n * (_n - 1) / 2 * (_a / _m)
      _a %= _m
    }
    if (_b >= _m) {
      ans += _n * (_b / _m)
      _b %= _m
    }

    val yMax = _a * _n + _b
    if (yMax < _m) { return ans }

    // y_max < m * (n + 1)
    // floor(y_max / m) <= n
    _n = yMax / _m
    _b = yMax % _m
    // swap(m, a)
    val tmp = _m
    _m = _a
    _a = tmp
  }

  // unreachable
  ans
}
