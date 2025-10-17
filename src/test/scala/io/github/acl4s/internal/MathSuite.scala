package io.github.acl4s.internal

import scala.collection.mutable

import io.github.acl4s.gcd

class MathSuite extends munit.FunSuite {

  test("Barrett") {
    (1 to 100).foreach(m => {
      val bt = Barrett(m)
      for {
        a <- 0 until m
        b <- 0 until m
      } {
        assertEquals(bt.mul(a, b), (a * b) % m)
      }
    })

    assertEquals(Barrett(1).mul(0, 0), 0)
  }

  test("Barrett Int border") {
    (0 to 20).foreach(d => {
      val mod = Int.MaxValue - d
      val lmod = mod.toLong
      val bt = Barrett(mod)
      val v = (0 until 10).flatMap(i =>
        Array(
          i,
          mod - i,
          mod / 2 + i,
          mod / 2 - i
        )
      )
      for (a <- v) {
        val la = a.toLong
        assertEquals(bt.mul(a, bt.mul(a, a)), (((la * la) % lmod * la) % lmod).toInt)
        for (b <- v) {
          val lb = b.toLong
          assertEquals(bt.mul(a, b), ((la * lb) % lmod).toInt)
        }
      }
    })
  }

  test("isPrime()") {
    assertEquals(isPrime(121), false)
    assertEquals(isPrime(11 * 13), false)
    assertEquals(isPrime(998_244_353), true)
    assertEquals(isPrime(1_000_000_007), true)
    assertEquals(isPrime(1_000_000_008), false)
    assertEquals(isPrime(1_000_000_009), true)
  }

  test("isPrime() convolution MODs") {
    import io.github.acl4s.Convolution.*

    assertEquals(isPrime(MOD1), true)
    assertEquals(isPrime(MOD2), true)
    assertEquals(isPrime(MOD3), true)
  }

  def buildBorderFromIsPrimeNaive(): Map[Int, Boolean] = {
    val map = (0 to 10_000)
      .flatMap(i =>
        Array(
          i -> true,
          (Int.MaxValue - i) -> true
        )
      )
      .to(mutable.TreeMap)
    map(0) = false
    map(1) = false

    (2 to 46_340 /* <= sqrt(Int.MaxValue) */ )
      .foreach(i => {
        val i2 = i * i
        map.foreach((n, b) => {
          if (b && i2 <= n && (n % i) == 0) {
            map(n) = false
          }
        })
      })

    map.toMap
  }

  test("isPrime() border") {
    val preds = buildBorderFromIsPrimeNaive()

    preds.foreach((x, expected) => {
      assertEquals(isPrime(x), expected)
    })
  }

  test("safeMod()") {
    val preds = (0L to 100L).flatMap(i =>
      Array(
        i,
        -i,
        Long.MinValue + i,
        Long.MaxValue - i
      )
    )

    for {
      a <- preds
      b <- preds
      if b > 0
    } {
      val ans = (BigInt(a) % b + b) % b
      assertEquals(safeMod(a, b), ans.toLong)
    }
  }

  test("invGcd() border") {
    val preds = (0L to 10L)
      .flatMap(i =>
        Array(
          i,
          -i,
          Long.MinValue + i,
          Long.MaxValue - i,
          Long.MinValue / 2 + i,
          Long.MinValue / 2 - i,
          Long.MaxValue / 2 + i,
          Long.MaxValue / 2 - i,
          Long.MinValue / 3 + i,
          Long.MinValue / 3 - i,
          Long.MaxValue / 3 + i,
          Long.MaxValue / 3 - i
        )
      )
      .to(mutable.ArrayBuffer)
    preds.addOne(998_244_353L)
    preds.addOne(1_000_000_007L)
    preds.addOne(1_000_000_009L)
    preds.addOne(-998_244_353L)
    preds.addOne(-1_000_000_007L)
    preds.addOne(-1_000_000_009L)

    for {
      a <- preds
      b <- preds
      if b > 0
    } {
      val a2 = safeMod(a, b)
      val LPair(g, x) = invGcd(a, b)
      val g2 = gcd(a2, b)

      assertEquals(g, g2)
      assert(x >= 0)
      assert(x <= b / g)
      assertEquals((BigInt(x) * a2 % b).toLong, g2 % b)
    }
  }

  def factors(m: Int): collection.IndexedSeq[Int] = {
    var _m = m
    val result = mutable.ArrayBuffer.empty[Int]
    Iterator
      .from(start = 2, step = 1)
      .takeWhile(i => i.toLong * i <= m)
      .filter(_m % _ == 0)
      .foreach(i => {
        result.addOne(i)
        while (_m % i == 0) { _m /= i }
      })
    if (_m > 1) { result.addOne(_m) }
    result
  }

  def isPrimitiveRoot(m: Int, g: Int): Boolean = {
    assert(1 <= g && g < m)
    factors(m - 1).forall(x => powMod(g, (m - 1) / x, m) != 1)
  }

  test("primitiveRoot test") {
    for {
      i <- 0 until 1_000
      x = Int.MaxValue - i
      if isPrime(x)
    } {
      assert(isPrimitiveRoot(x, primitiveRoot(x)))
    }
  }

  test("primitiveRoot constant") {
    // format: off
    assert(isPrimitiveRoot(            2, primitiveRoot(            2)))
    assert(isPrimitiveRoot(            3, primitiveRoot(            3)))
    assert(isPrimitiveRoot(            5, primitiveRoot(            5)))
    assert(isPrimitiveRoot(            7, primitiveRoot(            7)))
    assert(isPrimitiveRoot(           11, primitiveRoot(           11)))
    assert(isPrimitiveRoot(  998_244_353, primitiveRoot(  998_244_353)))
    assert(isPrimitiveRoot(1_000_000_007, primitiveRoot(1_000_000_007)))

    assert(isPrimitiveRoot(  469_762_049, primitiveRoot(  469_762_049)))
    assert(isPrimitiveRoot(  167_772_161, primitiveRoot(  167_772_161)))
    assert(isPrimitiveRoot(  754_974_721, primitiveRoot(  754_974_721)))
    assert(isPrimitiveRoot(  324_013_369, primitiveRoot(  324_013_369)))
    assert(isPrimitiveRoot(  831_143_041, primitiveRoot(  831_143_041)))
    assert(isPrimitiveRoot(1_685_283_601, primitiveRoot(1_685_283_601)))
    // format: on
  }

  test("primitiveRoot naive") {
    for {
      m <- 2 to 10_000
      if isPrime(m)
    } {
      val n = primitiveRoot(m)
      assert(1 <= n)
      assert(n < m)
      var x = 1
      for (i <- 1 to (m - 2)) {
        x = (x.toLong * n % m).toInt
        // x == n^i
        assertNotEquals(x, 1, s"m=$m, n=$n, i=$i")
      }
      x = (x.toLong * n % m).toInt
      assertEquals(x, 1, s"m=$m, n=$n")
    }
  }

}
