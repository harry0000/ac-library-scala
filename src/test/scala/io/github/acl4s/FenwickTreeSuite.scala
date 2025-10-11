package io.github.acl4s

import scala.annotation.nowarn

// for ModInt imports
@nowarn("msg=unused import")
class FenwickTreeSuite extends munit.FunSuite {

  /**
   * @see https://atcoder.jp/contests/practice2/tasks/practice2_b
   */
  test("AtCoder Library Practice Contest B - Fenwick Tree") {
    val fw = FenwickTree(Array(1L, 2L, 3L, 4L, 5L))

    assertEquals(fw.sum(0, 5), 15L)
    assertEquals(fw.sum(2, 4), 7L)

    fw.add(3, 10)

    assertEquals(fw.sum(0, 5), 25L)
    assertEquals(fw.sum(0, 3), 6L)
  }

  test("zero") {
    {
      val fw = FenwickTree[Long](0)
      assertEquals(fw.sum(0, 0), 0L)
    }

    {
      import DynamicModInt as ModInt

      val fw = FenwickTree[ModInt](0)
      assertEquals(fw.sum(0, 0), ModInt(0))
    }

    {
      import ModInt998244353 as ModInt

      val fw = FenwickTree[ModInt](0)
      assertEquals(fw.sum(0, 0), ModInt(0))
    }

    {
      import ModInt1000000007 as ModInt

      val fw = FenwickTree[ModInt](0)
      assertEquals(fw.sum(0, 0), ModInt(0))
    }

    {
      given Modulus[1_000_000_009] = Modulus[1_000_000_009]()
      type ModInt = StaticModInt[1_000_000_009]
      val ModInt = StaticModInt

      val fw = FenwickTree[ModInt](0)
      assertEquals(fw.sum(0, 0), ModInt(0))
    }
  }

  test("naive") {
    (0 to 50).foreach(n => {
      val fw = FenwickTree[Long](n)
      (0 until n).foreach(i => {
        fw.add(i, i.toLong * i)
      })

      for {
        l <- 0 to n
        r <- l to n
      } {
        val sum = (l until r).map(i => i.toLong * i).sum
        assertEquals(fw.sum(l, r), sum)
      }
    })
  }

  test("bound int") {
    val fw = FenwickTree[Int](10)

    fw.add(3, Int.MaxValue)
    fw.add(5, Int.MinValue)

    assertEquals(fw.sum(0, 10), -1)
    assertEquals(fw.sum(3, 6), -1)

    assertEquals(fw.sum(3, 4), Int.MaxValue)
    assertEquals(fw.sum(4, 10), Int.MinValue)
  }

  test("bound long") {
    val fw = FenwickTree[Long](10)

    fw.add(3, Long.MaxValue)
    fw.add(5, Long.MinValue)

    assertEquals(fw.sum(0, 10), -1L)
    assertEquals(fw.sum(3, 6), -1L)

    assertEquals(fw.sum(3, 4), Long.MaxValue)
    assertEquals(fw.sum(4, 10), Long.MinValue)
  }

  test("overflow") {
    val fw = FenwickTree[Int](20)
    val a = new Array[Long](20)
    (0 until 10).foreach(i => {
      fw.add(i, Int.MaxValue)
      a(i) += Int.MaxValue
    })
    (10 until 20).foreach(i => {
      fw.add(i, Int.MinValue)
      a(i) += Int.MinValue
    })

    fw.add(5, 11_111)
    a(5) += 11_111

    for {
      l <- 0 to 20
      r <- l to 20
    } {
      val sum = (l until r).map(i => a(i)).sum
      val dif = sum - fw.sum(l, r)
      assertEquals(dif % (1L << 32), 0L)
    }
  }

  test("StaticModInt") {
    given Modulus[11] = Modulus[11]()
    type ModInt = StaticModInt[11]
    val ModInt = StaticModInt

    (0 to 50).foreach(n => {
      val fw = FenwickTree[ModInt](n)
      (0 until n).foreach(i => {
        fw.add(i, ModInt(i.toLong * i))
      })

      for {
        l <- 0 to n
        r <- l to n
      } {
        val sum = (l until r).map(i => i.toLong * i).sum
        assertEquals(fw.sum(l, r), ModInt(sum))
      }
    })
  }

  test("DynamicModInt") {
    import DynamicModInt as ModInt

    ModInt.setMod(11)

    (0 to 50).foreach(n => {
      val fw = FenwickTree[ModInt](n)
      (0 until n).foreach(i => {
        fw.add(i, ModInt(i.toLong * i))
      })

      for {
        l <- 0 to n
        r <- l to n
      } {
        val sum = (l until r).map(i => i.toLong * i).sum
        assertEquals(fw.sum(l, r), ModInt(sum))
      }
    })
  }

}
