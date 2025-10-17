package io.github.acl4s

import scala.annotation.nowarn
import scala.util.Random

// for Mint imports
@nowarn("msg=unused import")
class ConvolutionSuite extends munit.FunSuite {

  /**
   * @see https://atcoder.jp/contests/practice2/tasks/practice2_f
   */
  test("AtCoder Library Practice Contest F - Convolution") {
    import ModInt998244353 as Mint

    // Sample Input 1
    {
      // val n = 4
      // val m = 5
      val a = Array(1, 2, 3, 4)
      val b = Array(5, 6, 7, 8, 9)

      val _a = a.map(Mint(_))
      val _b = b.map(Mint(_))

      val expected = IndexedSeq(5, 16, 34, 60, 70, 70, 59, 36)
      val obtained = convolution(_a, _b).map(_.value)

      assertEquals(obtained, expected)
    }
    // Sample Input 2
    {
      // val n = 1
      // val m = 1
      val a = Array(10_000_000)
      val b = Array(10_000_000)

      val _a = a.map(Mint(_))
      val _b = b.map(Mint(_))

      val expected = IndexedSeq(871_938_225)
      val obtained = convolution(_a, _b).map(_.value)

      assertEquals(obtained, expected)
    }
  }

  def convNaive[M <: Int](
    a: Array[StaticModInt[M]],
    b: Array[StaticModInt[M]]
  )(using Modulus[M]): collection.IndexedSeq[StaticModInt[M]] = {
    val n = a.length
    val m = b.length
    val c = Array.fill(n + m - 1)(StaticModInt())
    for {
      i <- 0 until n
      j <- 0 until m
    } {
      c(i + j) += a(i) * b(j)
    }
    c
  }

  test("convolution empty") {
    import ModInt998244353 as Mint

    assertEquals(convolution(Array.empty[Mint], Array.empty[Mint]), IndexedSeq.empty[Mint])
    assertEquals(convolution(Array.empty[Mint], Array(Mint(1), Mint(2))), IndexedSeq.empty[Mint])
    assertEquals(convolution(Array(Mint(1), Mint(2)), Array.empty[Mint]), IndexedSeq.empty[Mint])
  }

  test("convolution mid") {
    import ModInt998244353 as Mint
    val n = 1234
    val m = 2345

    val a = Array.fill(n)(Mint(Random.nextInt()))
    val b = Array.fill(m)(Mint(Random.nextInt()))

    assertEquals(convolution(a, b), convNaive(a, b))
  }

  test("convolution simple SMod") {
    import StaticModInt as Mint

    type Mod1 = 998_244_353
    type Mod2 = 924_844_033
    given mod998_244_353Modulus: Modulus[Mod1] = Modulus[Mod1]()
    given mod924_844_033Modulus: Modulus[Mod2] = Modulus[Mod2]()

    for {
      n <- 1 to 20
      m <- 1 to 20
    } {
      val a = Array.fill(n)(Mint[Mod1]())
      val b = Array.fill(m)(Mint[Mod1]())

      assertEquals(convolution(a, b), convNaive(a, b))
    }
    for {
      n <- 1 to 20
      m <- 1 to 20
    } {
      val a = Array.fill(n)(Mint[Mod2]())
      val b = Array.fill(m)(Mint[Mod2]())

      assertEquals(convolution(a, b), convNaive(a, b))
    }
  }

  def convLongNaive(a: Array[Long], b: Array[Long]): collection.IndexedSeq[Long] = {
    val n = a.length
    val m = b.length
    val c = new Array[Long](n + m - 1)
    for {
      i <- 0 until n
      j <- 0 until m
    } {
      c(i + j) += a(i) * b(j)
    }
    c
  }

  test("convolutionLong empty") {
    assertEquals(convolutionLong(Array.empty[Long], Array.empty[Long]), IndexedSeq.empty)
    assertEquals(convolutionLong(Array.empty[Long], Array(1L, 2L)), IndexedSeq.empty)
    assertEquals(convolutionLong(Array(1L, 2L), Array.empty[Long]), IndexedSeq.empty)
  }

  test("convolutionLong") {
    for {
      n <- 1 to 20
      m <- 1 to 20
    } {
      val a = Array.fill(n)((Random.nextInt() % 1_000_000).toLong - 500_000)
      val b = Array.fill(m)((Random.nextInt() % 1_000_000).toLong - 500_000)

      assertEquals(convolutionLong(a, b), convLongNaive(a, b))
    }
  }

  test("convolutionLong bound") {
    import io.github.acl4s.Convolution.*

    for (i <- -1000 to 1000) {
      val a = Array(0L - M1M2 - M1M3 - M2M3 + i)
      val b = Array(1L)

      assertEquals(convolutionLong(a, b), a.toIndexedSeq)
    }
    for (i <- -1000 to 1000) {
      val a = Array(Long.MinValue + i)
      val b = Array(1L)

      assertEquals(convolutionLong(a, b), a.toIndexedSeq)
    }
    for (i <- -1000 to 1000) {
      val a = Array(Long.MaxValue - i)
      val b = Array(1L)

      assertEquals(convolutionLong(a, b), a.toIndexedSeq)
    }
  }

}
