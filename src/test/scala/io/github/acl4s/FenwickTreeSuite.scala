package io.github.acl4s

class FenwickTreeSuite extends BaseSuite {

  /**
   * @see https://atcoder.jp/contests/practice2/tasks/practice2_b
   */
  test("AtCoder Library Practice Contest B - Fenwick Tree") {
    val fw = FenwickTree(Array(1L, 2L, 3L, 4L, 5L))

    assert(fw.sum(0, 5) === 15L)
    assert(fw.sum(2, 4) === 7L)

    fw.add(3, 10)

    assert(fw.sum(0, 5) === 25L)
    assert(fw.sum(0, 3) === 6L)
  }

  test("zero") {
    {
      val fw = FenwickTree[Long](0)
      assert(fw.sum(0, 0) === 0L)
    }

    {
      type ModInt = DynamicModInt
      val ModInt = DynamicModInt

      val fw = FenwickTree[ModInt](0)
      assert(fw.sum(0, 0) === ModInt(0))
    }

    {
      type ModInt = ModInt998244353
      val ModInt = ModInt998244353

      val fw = FenwickTree[ModInt](0)
      assert(fw.sum(0, 0) === ModInt(0))
    }

    {
      type ModInt = ModInt1000000007
      val ModInt = ModInt1000000007

      val fw = FenwickTree[ModInt](0)
      assert(fw.sum(0, 0) === ModInt(0))
    }

    {
      given Modulus[1_000_000_009] = Modulus[1_000_000_009]()
      type ModInt = StaticModInt[1_000_000_009]
      val ModInt = StaticModInt

      val fw = FenwickTree[ModInt](0)
      assert(fw.sum(0, 0) === ModInt(0))
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
        assert(fw.sum(l, r) === sum)
      }
    })
  }

  test("bound int") {
    val fw = FenwickTree[Int](10)

    fw.add(3, Int.MaxValue)
    fw.add(5, Int.MinValue)

    assert(fw.sum(0, 10) === -1)
    assert(fw.sum(3, 6) === -1)

    assert(fw.sum(3, 4) === Int.MaxValue)
    assert(fw.sum(4, 10) === Int.MinValue)
  }

  test("bound long") {
    val fw = FenwickTree[Long](10)

    fw.add(3, Long.MaxValue)
    fw.add(5, Long.MinValue)

    assert(fw.sum(0, 10) === -1L)
    assert(fw.sum(3, 6) === -1L)

    assert(fw.sum(3, 4) === Long.MaxValue)
    assert(fw.sum(4, 10) === Long.MinValue)
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
      assert(dif % (1L << 32) === 0L)
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
        assert(fw.sum(l, r) === ModInt(sum))
      }
    })
  }

  test("DynamicModInt") {
    type ModInt = DynamicModInt
    val ModInt = DynamicModInt
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
        assert(fw.sum(l, r) === ModInt(sum))
      }
    })
  }

}
