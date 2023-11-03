package io.github.acl4s

import io.github.acl4s.internal.{invGcd, safeMod, Barrett, LPair}

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
