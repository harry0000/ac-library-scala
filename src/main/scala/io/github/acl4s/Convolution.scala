package io.github.acl4s

import StaticModInt as ModInt

import scala.reflect.ClassTag

def convolution[M <: Int](
  a: Array[ModInt[M]],
  b: Array[ModInt[M]]
)(using Modulus[M]): collection.IndexedSeq[ModInt[M]] = {
  if (a.isEmpty || b.isEmpty) {
    return IndexedSeq.empty
  }

  val n = a.length
  val m = b.length

  if (n.min(m) <= 60) {
    convolutionNaive(a, b)
  } else {
    convolutionFft(a, b)
  }
}

private[acl4s] def convolutionNaive[M <: Int](
  a: Array[ModInt[M]],
  b: Array[ModInt[M]]
)(using Modulus[M]): collection.IndexedSeq[ModInt[M]] = {
  val n = a.length
  val m = b.length
  val ans = Array.fill(n + m - 1)(ModInt())
  if (n < m) {
    for {
      j <- 0 until m
      i <- 0 until n
    } {
      ans(i + j) += a(i) * b(j)
    }
  } else {
    for {
      i <- 0 until n
      j <- 0 until m
    } {
      ans(i + j) += a(i) * b(j)
    }
  }
  ans
}

private[acl4s] def convolutionFft[M <: Int](
  a: Array[ModInt[M]],
  b: Array[ModInt[M]]
)(using Modulus[M]): collection.IndexedSeq[ModInt[M]] = {
  val n = a.length
  val m = b.length
  val z = 1 << internal.ceilPow2(n + m - 1)

  val _a = java.util.Arrays.copyOf(a, z)
  (n until z).foreach(i => { _a(i) = ModInt() })
  butterfly(_a)

  val _b = java.util.Arrays.copyOf(b, z)
  (m until z).foreach(i => { _b(i) = ModInt() })
  butterfly(_b)

  for (i <- 0 until z) {
    _a(i) *= _b(i)
  }
  butterflyInv(_a)

  val ans = java.util.Arrays.copyOf(_a, n + m - 1)
  (z until n + m - 1).foreach(i => { ans(i) = ModInt() })
  val iz = ModInt(z).inv
  for (i <- 0.until(n + m - 1)) {
    ans(i) *= iz
  }
  ans
}

private def butterfly[M <: Int](a: Array[ModInt[M]])(using m: Modulus[M]): Unit = {
  val n = a.length
  val h = java.lang.Integer.numberOfTrailingZeros(n)

  val info = FftInfo[M]()

  var len = 0 // a[i, i+(n>>len), i+2*(n>>len), ..] is transformed
  while (len < h) {
    if (h - len == 1) {
      val p = 1 << (h - len - 1)
      val rot = ModInt(1)
      for (s <- 0 until (1 << len)) {
        val offset = s << (h - len)
        for (i <- 0 until p) {
          val l = a(i + offset)
          val r = a(i + offset + p) * rot
          a(i + offset) = l + r
          a(i + offset + p) = l - r
        }
        if (s + 1 != (1 << len)) {
          rot *= info.rate2(java.lang.Integer.numberOfTrailingZeros(~s))
        }
      }
      len += 1
    } else {
      // 4-base
      val p = 1 << (h - len - 2)
      val rot = ModInt(1)
      val imag = info.root(2)
      for (s <- 0 until (1 << len)) {
        val rot2 = rot * rot
        val rot3 = rot2 * rot
        val offset = s << (h - len)
        for (i <- 0 until p) {
          val mod2 = m.value.toLong * m.value
          val a0 = a(i + offset).value.toLong
          val a1 = a(i + offset + p).value.toLong * rot.value
          val a2 = a(i + offset + 2 * p).value.toLong * rot2.value
          val a3 = a(i + offset + 3 * p).value.toLong * rot3.value
          val a1na3imag = ModInt(a1 + mod2 - a3).value.toLong * imag.value
          val na2 = mod2 - a2
          a(i + offset) = ModInt(a0 + a1 + a2 + a3)
          a(i + offset + 1 * p) = ModInt(a0 + a2 + (2 * mod2 - (a1 + a3)))
          a(i + offset + 2 * p) = ModInt(a0 + na2 + a1na3imag)
          a(i + offset + 3 * p) = ModInt(a0 + na2 + (mod2 - a1na3imag))
        }
        if (s + 1 != (1 << len)) {
          rot *= info.rate3(java.lang.Integer.numberOfTrailingZeros(~s))
        }
      }
      len += 2
    }
  }
}

private def butterflyInv[M <: Int](a: Array[ModInt[M]])(using m: Modulus[M]): Unit = {
  val n = a.length
  val h = java.lang.Integer.numberOfTrailingZeros(n)

  val info = FftInfo[M]()

  var len = h
  while (len > 0) {
    if (len == 1) {
      val p = 1 << (h - len)
      val iRot = ModInt(1)
      for (s <- 0 until (1 << (len - 1))) {
        val offset = s << (h - len + 1)
        for (i <- 0 until p) {
          val l = a(i + offset)
          val r = a(i + offset + p)
          a(i + offset) = l + r
          a(i + offset + p) = (l - r + ModInt(m.value)) * iRot
        }
        if (s + 1 != (1 << (len - 1))) {
          iRot *= info.iRate2(java.lang.Integer.numberOfTrailingZeros(~s))
        }
      }
      len -= 1
    } else {
      // 4-base
      val p = 1 << (h - len)
      val iRot = ModInt(1)
      val iImag = info.iRoot(2)
      for (s <- 0 until (1 << (len - 2))) {
        val iRot2 = iRot * iRot
        val iRot3 = iRot2 * iRot
        val offset = s << (h - len + 2)
        for (i <- 0 until p) {
          val a0 = a(i + offset + 0 * p).value.toLong
          val a1 = a(i + offset + 1 * p).value.toLong
          val a2 = a(i + offset + 2 * p).value.toLong
          val a3 = a(i + offset + 3 * p).value.toLong

          val a2na3iImag = (ModInt(m.value + a2 - a3) * iImag).value.toLong

          a(i + offset) = ModInt(a0 + a1 + a2 + a3)
          a(i + offset + 1 * p) = ModInt(a0 + (m.value - a1) + a2na3iImag) * iRot
          a(i + offset + 2 * p) = ModInt(a0 + a1 + (m.value - a2) + (m.value - a3)) * iRot2
          a(i + offset + 3 * p) = ModInt(a0 + (m.value - a1) + (m.value - a2na3iImag)) * iRot3
        }
        if (s + 1 != (1 << (len - 2))) {
          iRot *= info.iRate3(java.lang.Integer.numberOfTrailingZeros(~s))
        }
      }
      len -= 2
    }
  }
}

def convolutionLong(
  a: Array[Long],
  b: Array[Long]
): collection.IndexedSeq[Long] = {
  val n = a.length
  val m = b.length

  if (a.isEmpty || b.isEmpty) {
    return IndexedSeq.empty
  }

  import Convolution.*

  assert(n + m - 1 <= (1 << MAX_AB_BIT))

  val c1 = convolutionLongMod[MOD1.type](a, b)
  val c2 = convolutionLongMod[MOD2.type](a, b)
  val c3 = convolutionLongMod[MOD3.type](a, b)

  val c = new Array[Long](n + m - 1)
  for (i <- 0.until(n + m - 1)) {
    var x = 0L
    x += (c1(i) * I1) % MOD1 * M2M3
    x += (c2(i) * I2) % MOD2 * M1M3
    x += (c3(i) * I3) % MOD3 * M1M2
    // B = 2^63, -B <= x, r(real value) < B
    // (x, x - M, x - 2M, or x - 3M) = r (mod 2B)
    // r = c1[i] (mod MOD1)
    // focus on MOD1
    // r = x, x - M', x - 2M', x - 3M' (M' = M % 2^64) (mod 2B)
    // r = x,
    //     x - M' + (0 or 2B),
    //     x - 2M' + (0, 2B or 4B),
    //     x - 3M' + (0, 2B, 4B or 6B) (without mod!)
    // (r - x) = 0, (0)
    //           - M' + (0 or 2B), (1)
    //           -2M' + (0 or 2B or 4B), (2)
    //           -3M' + (0 or 2B or 4B or 6B) (3) (mod MOD1)
    // we checked that
    //   ((1) mod MOD1) mod 5 = 2
    //   ((2) mod MOD1) mod 5 = 3
    //   ((3) mod MOD1) mod 5 = 4
    val diff = {
      val diff = c1(i) - internal.safeMod(x, MOD1.toLong)
      if (diff < 0L) { diff + MOD1 }
      else { diff }
    }
    x -= Offset((diff % 5 /* == Offset.length */ ).toInt)
    c(i) = x
  }

  c
}

private def convolutionLongMod[M <: Int](
  a: Array[Long],
  b: Array[Long]
)(using mod: Modulus[M]): collection.IndexedSeq[Int] = {
  val n = a.length
  val m = b.length
  if (n == 0 || m == 0) {
    return IndexedSeq.empty
  }

  val z = 1 << internal.ceilPow2(n + m - 1)
  assert((mod.value - 1) % z == 0)

  val a2 = a.map(ModInt(_))
  val b2 = b.map(ModInt(_))
  val c2 = convolution(a2, b2)

  c2.map(_.value)
}

final class FftInfo[M <: Int] private (using m: Modulus[M]) {
  private val g: Int = internal.primitiveRoot(m.value)
  private val rank2: Int = java.lang.Integer.numberOfTrailingZeros(m.value - 1)

  val root: Array[ModInt[M]] = Array.fill(rank2 + 1)(ModInt()) // root[i]^(2^i) == 1
  val iRoot: Array[ModInt[M]] = Array.fill(rank2 + 1)(ModInt()) // root[i] * iroot[i] == 1

  val rate2: Array[ModInt[M]] = Array.fill(0.max(rank2 - 2 + 1))(ModInt())
  val iRate2: Array[ModInt[M]] = Array.fill(0.max(rank2 - 2 + 1))(ModInt())

  val rate3: Array[ModInt[M]] = Array.fill(0.max(rank2 - 3 + 1))(ModInt())
  val iRate3: Array[ModInt[M]] = Array.fill(0.max(rank2 - 3 + 1))(ModInt())

  root(rank2) = ModInt(g).pow((m.value - 1) >> rank2)
  iRoot(rank2) = root(rank2).inv
  for (i <- (rank2 - 1) to 0 by -1) {
    root(i) = root(i + 1) * root(i + 1)
    iRoot(i) = iRoot(i + 1) * iRoot(i + 1)
  }

  {
    val prod = ModInt(1)
    val iProd = ModInt(1)
    for (i <- 0 to (rank2 - 2)) {
      rate2(i) = root(i + 2) * prod
      iRate2(i) = iRoot(i + 2) * iProd
      prod *= iRoot(i + 2)
      iProd *= root(i + 2)
    }
  }
  {
    val prod = ModInt(1)
    val iProd = ModInt(1)
    for (i <- 0 to (rank2 - 3)) {
      rate3(i) = root(i + 3) * prod
      iRate3(i) = iRoot(i + 3) * iProd
      prod *= iRoot(i + 3)
      iProd *= root(i + 3)
    }
  }
}
object FftInfo {
  import scala.collection.mutable

  private val cache: mutable.Map[Int, FftInfo[?]] = mutable.Map.empty

  def apply[M <: Int]()(using m: Modulus[M]): FftInfo[M] = {
    cache.getOrElseUpdate(m.value, new FftInfo()).asInstanceOf[FftInfo[M]]
  }
}

private[acl4s] object Convolution {
  import scala.compiletime.ops.int.{`*`, `+`, `<<`}

  val MAX_AB_BIT: 24 = 24

  val MOD1: (1 << 24) * 45 + 1 = 754_974_721
  val MOD2: (1 << 25) * 5 + 1 = 167_772_161
  val MOD3: (1 << 26) * 7 + 1 = 469_762_049

  // for Scala 3.3
  val M2M3: Long = MOD2.toLong * MOD3
  val M1M3: Long = MOD1.toLong * MOD3
  val M1M2: Long = MOD1.toLong * MOD2

  private val M1M2M3: Long = MOD1.toLong * MOD2 * MOD3

  // for Scala 3.7 and later
  // import scala.compiletime.ops.int.ToLong
  // import scala.compiletime.ops.long.`*` as x
  //
  // val M2M3: ToLong[MOD2.type] x ToLong[MOD3.type] = MOD2.toLong * MOD3
  // val M1M3: ToLong[MOD1.type] x ToLong[MOD3.type] = MOD1.toLong * MOD3
  // val M1M2: ToLong[MOD1.type] x ToLong[MOD2.type] = MOD1.toLong * MOD2
  //
  // private val M1M2M3: M1M2.type x ToLong[MOD3.type] = MOD1.toLong * MOD2 * MOD3

  val I1: Long = internal.invGcd(M2M3, MOD1.toLong).b
  val I2: Long = internal.invGcd(M1M3, MOD2.toLong).b
  val I3: Long = internal.invGcd(M1M2, MOD3.toLong).b

  given Modulus[MOD1.type] = Modulus()
  given Modulus[MOD2.type] = Modulus()
  given Modulus[MOD3.type] = Modulus()

  val Offset: IArray[Long] = IArray(
    0L,
    0L,
    M1M2M3,
    2 * M1M2M3,
    3 * M1M2M3
  )
}
