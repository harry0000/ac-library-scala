package io.github.acl4s

import scala.collection.mutable

class MathSuite extends munit.FunSuite {

  test("crt() hand") {
    val obtained = crt(Array(1L, 2L, 1L), Array(2L, 3L, 2L))

    assertEquals(obtained, (5L, 6L))
  }

  test("crt()") {
    {
      for {
        a <- 1L to 20L
        b <- 1L to 20L
        c <- -10L to 10L
        d <- -10L to 10L
      } {
        val res = crt(Array(c, d), Array(a, b))

        if (res._2 == 0) {
          for (x <- 0L until (a * b / gcd(a, b))) {
            assert(x % a != c || x % b != d)
          }
        } else {
          assertEquals(res._2, a * b / gcd(a, b))
          assertEquals(res._1 % a, internal.safeMod(c, a))
          assertEquals(res._1 % b, internal.safeMod(d, b))
        }
      }
    }
    {
      for {
        a <- 1L to 5L
        b <- 1L to 5L
        c <- 1L to 5L
        d <- -5L to 5L
        e <- -5L to 5L
        f <- -5L to 5L
      } {
        val res = crt(Array(d, e, f), Array(a, b, c))
        val lcm = {
          val lcm = a * b / gcd(a, b)
          lcm * c / gcd(lcm, c)
        }

        if (res._2 == 0) {
          for (x <- 0L until lcm) {
            assert(x % a != d || x % b != e || x % c != f)
          }
        } else {
          assertEquals(res._2, lcm)
          assertEquals(res._1 % a, internal.safeMod(d, a))
          assertEquals(res._1 % b, internal.safeMod(e, b))
          assertEquals(res._1 % c, internal.safeMod(f, c))
        }
      }
    }
  }

  test("crt() overflow") {
    val r0 = 0L
    val r1 = 1_000_000_000_000L - 2L
    val m0 = 900_577L
    val m1 = 1_000_000_000_000L

    val (rem, mod) = crt(Array(r0, r1), Array(m0, m1))

    assertEquals(mod, m0 * m1)
    assertEquals(rem % m0, r0)
    assertEquals(rem % m1, r1)
  }

  test("crt() bound") {
    val INF = Long.MaxValue
    val pred = mutable.ArrayBuffer.empty[Long]
    for (i <- 1L to 10L) {
      pred.addOne(i)
      pred.addOne(INF - (i - 1))
    }
    pred.addOne(998_244_353L)
    pred.addOne(1_000_000_007L)
    pred.addOne(1_000_000_009L)

    for {
      // format: off
      ab <- Seq((INF, INF),
                (1L, INF),
                (7L, INF),
                (337L, INF / 337L),
                (2L, (INF - 1L) / 2L))
      // format: on
      (a, b) <- Seq(ab, ab.swap)
      ans <- pred
    } {
      val res = crt(Array(ans % a, ans % b), Array(a, b))
      val lcm = a / gcd(a, b) * b

      assertEquals(res._2, lcm)
      assertEquals(res._1, ans % lcm)
    }
    {
      val factorInf = Seq(49L, 73L, 127L, 337L, 92_737L, 649_657L)
      for {
        factors <- factorInf.permutations
        ans <- pred
      } {
        val r = factors.map(ans % _).toArray
        val m = factors.toArray

        val res = crt(r, m)
        assertEquals(res._1, ans % INF)
        assertEquals(res._2, INF)
      }
    }
    {
      val factorInf = Seq(2L, 3L, 715_827_883L, 2_147_483_647L)
      for {
        factors <- factorInf.permutations
        ans <- pred
      } {
        val r = factors.map(ans % _).toArray
        val m = factors.toArray

        val res = crt(r, m)
        assertEquals(res._1, ans % (INF - 1))
        assertEquals(res._2, INF - 1)
      }
    }
  }

  test("floorSum()") {
    def floorSumNaive(n: Int, m: Long, a: Long, b: Long): Long = {
      (0 until n).map(i => Math.floorDiv(a * i + b, m)).sum
    }

    for {
      n <- 0L to 20L
      m <- 1L to 20L
      a <- -20L to 20L
      b <- -20L to 20L
    } {
      assertEquals(floorSum(n, m, a, b), floorSumNaive(n.toInt, m, a, b), s"n=$n, m=$m, a=$a, b=$b")
    }
  }

}
