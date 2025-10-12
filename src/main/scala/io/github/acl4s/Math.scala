package io.github.acl4s

import io.github.acl4s.internal.{floorSumUnsigned, invGcd, safeMod, Barrett, LPair}

def powMod(x: Long, n: Long, m: Int): Int = {
  assert(0L <= n && 1 <= m)
  if (m == 1) { return 0 }
  val bt = Barrett(m)
  var _n = n
  var r = 1
  var y = safeMod(x, m.toLong).toInt
  while (_n > 0L) {
    if ((_n & 1) != 0) { r = bt.mul(r, y) }
    y = bt.mul(y, y)
    _n >>= 1
  }
  r
}

def invMod(x: Long, m: Long): Long = {
  assert(1L <= m);
  val LPair(z, inv) = invGcd(x, m);
  assert(z == 1L)
  inv
}

def floorSum(n: Long, m: Long, a: Long, b: Long): Long = {
  require(0L <= n && n < (1L << 32))
  require(1L <= m && m < (1L << 32))

  var ans = 0L
  var _a = a
  var _b = b

  if (_a < 0) {
    val a2 = safeMod(_a, m)
    ans -= (n * (n - 1) / 2) * ((a2 - _a) / m)
    _a = a2
  }
  if (_b < 0) {
    val b2 = safeMod(_b, m)
    ans -= n * ((b2 - _b) / m)
    _b = b2
  }

  ans + floorSumUnsigned(n, m, _a, _b)
}
