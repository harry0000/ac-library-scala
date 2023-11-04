package io.github.acl4s.internal

import scala.collection.mutable

import io.github.acl4s.{BaseSuite, gcd}

class MathSuite extends BaseSuite {

  test("Barrett") {
    (1 to 100).foreach(m => {
      val bt = Barrett(m)
      for {
        a <- 0 until m
        b <- 0 until m
      } {
        assert(bt.mul(a, b) === (a * b) % m)
      }
    })

    assert(Barrett(1).mul(0, 0) === 0)
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
        assert(bt.mul(a, bt.mul(a, a)) === (((la * la) % lmod * la) % lmod).toInt)
        for (b <- v) {
          val lb = b.toLong
          assert(bt.mul(a, b) === ((la * lb) % lmod).toInt)
        }
      }
    })
  }

  test("isPrime()") {
    assert(isPrime(121) === false)
    assert(isPrime(11 * 13) === false)
    assert(isPrime(998_244_353) === true)
    assert(isPrime(1_000_000_007) === true)
    assert(isPrime(1_000_000_008) === false)
    assert(isPrime(1_000_000_009) === true)
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
      assert(isPrime(x) === expected)
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
      assert(safeMod(a, b) === ans.toLong)
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

      assert(g === g2)
      assert(x >= 0)
      assert(x <= b / g)
      assert((BigInt(x) * a2 % b).toLong === g2 % b)
    }
  }

}
