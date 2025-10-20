package io.github.acl4s

import io.github.acl4s.internal.foreach

def powMod(x: Long, n: Long, m: Int): Int = {
  require(0L <= n && 1 <= m)
  if (m == 1) { return 0 }
  val bt = internal.Barrett(m)
  var _n = n
  var r = 1
  var y = internal.safeMod(x, m.toLong).toInt
  while (_n > 0L) {
    if ((_n & 1) != 0) { r = bt.mul(r, y) }
    y = bt.mul(y, y)
    _n >>= 1
  }
  r
}

def invMod(x: Long, m: Long): Long = {
  require(1L <= m)
  val (z, inv) = internal.invGcd(x, m)
  assert(z == 1L)
  inv
}

/**
 * @return (rem, mod)
 */
def crt(
  r: collection.IndexedSeq[Long],
  m: collection.IndexedSeq[Long]
): (Long, Long) = {
  require(r.length == m.length)
  val n = r.length

  // Contracts: 0 <= r0 < m0
  var r0 = 0L
  var m0 = 1L
  foreach(0 until n)(i => {
    require(1 <= m(i))
    val (r1, m1) = {
      val r1 = internal.safeMod(r(i), m(i))
      val m1 = m(i)
      if (m0 < m1) {
        // std::swap(r0, r1);
        // std::swap(m0, m1);
        val tmpR = r0
        val tmpM = m0
        r0 = r1
        m0 = m1

        (tmpR, tmpM)
      } else {
        (r1, m1)
      }
    }

    if (m0 % m1 == 0) {
      if (r0 % m1 != r1) { return (0L, 0L) }
      // continue;
    } else {
      // assume: m0 > m1, lcm(m0, m1) >= 2 * max(m0, m1)

      // (r0, m0), (r1, m1) -> (r2, m2 = lcm(m0, m1));
      // r2 % m0 = r0
      // r2 % m1 = r1
      // -> (r0 + x*m0) % m1 = r1
      // -> x*u0*g = r1-r0 (mod u1*g) (u0*g = m0, u1*g = m1)
      // -> x = (r1 - r0) / g * inv(u0) (mod u1)

      // im = inv(u0) (mod u1) (0 <= im < u1)
      val (g, im) = internal.invGcd(m0, m1)

      val u1 = m1 / g
      // |r1 - r0| < (m0 + m1) <= lcm(m0, m1)
      if ((r1 - r0) % g != 0L) { return (0L, 0L) }

      // u1 * u1 <= m1 * m1 / g / g <= m0 * m1 / g = lcm(m0, m1)
      val x = (r1 - r0) / g % u1 * im % u1

      // |r0| + |m0 * x|
      // < m0 + m0 * (u1 - 1)
      // = m0 + m0 * m1 / g - m0
      // = lcm(m0, m1)
      r0 += x * m0
      m0 *= u1 // -> lcm(m0, m1)
      if (r0 < 0) { r0 += m0 }
    }
  })
  (r0, m0)
}

def floorSum(n: Long, m: Long, a: Long, b: Long): Long = {
  require(0L <= n && n < (1L << 32))
  require(1L <= m && m < (1L << 32))

  var ans = 0L
  var _a = a
  var _b = b

  if (_a < 0) {
    val a2 = internal.safeMod(_a, m)
    ans -= (n * (n - 1) / 2) * ((a2 - _a) / m)
    _a = a2
  }
  if (_b < 0) {
    val b2 = internal.safeMod(_b, m)
    ans -= n * ((b2 - _b) / m)
    _b = b2
  }

  ans + internal.floorSumUnsigned(n, m, _a, _b)
}
