package io.github.acl4s

class StringSuite extends munit.FunSuite {

  /**
   * @see https://atcoder.jp/contests/practice2/tasks/practice2_i
   */
  test("AtCoder Library Practice Contest I - Number of Substrings") {
    def solve(s: String): Long = {
      val n = s.length

      val sa = suffixArray(s)
      val lcp = lcpArray(s, sa)

      val total = n.toLong * (n + 1) / 2
      val sum = lcp.foldLeft(0L)(_ + _)

      total - sum
    }

    // Sample Input 1
    {
      val s = "abcbcba"
      val expected = 21L

      val actual = solve(s)

      assertEquals(actual, expected)
    }
    // Sample Input 2
    {
      val s = "mississippi"
      val expected = 53L

      val actual = solve(s)

      assertEquals(actual, expected)
    }
    // Sample Input 3
    {
      val s = "ababacaca"
      val expected = 33L

      val actual = solve(s)

      assertEquals(actual, expected)
    }
    // Sample Input 4
    {
      val s = "aaaaa"
      val expected = 5L

      val actual = solve(s)

      assertEquals(actual, expected)
    }
  }

  private def suffixArrayNaive(s: Array[Int]): Array[Int] = {
    val n = s.length
    val sa = Array.tabulate(n)(identity)

    import scala.math.Ordering.Implicits.seqOrdering

    sa.sortInPlaceBy(i => s.slice(i, n).toIndexedSeq)
    sa
  }

  private def lcpArrayNaive(s: Array[Int], sa: Array[Int]): Array[Int] = {
    val n = s.length
    assert(n > 0)
    val lcp = new Array[Int](n - 1)
    for (i <- 0 until n - 1) {
      val l = sa(i)
      val r = sa(i + 1)
      while (l + lcp(i) < n && lcp(i) < n && s(l + lcp(i)) == s(r + lcp(i))) {
        lcp(i) += 1
      }
    }
    lcp
  }

  test("suffixArray empty") {
    assertEquals(suffixArray("").toSeq, Seq.empty[Int])
    assertEquals(suffixArray(Array.empty[Int]).toSeq, Seq.empty[Int])
    assertEquals(suffixArray(Array.empty[Long]).toSeq, Seq.empty[Int])
  }

  test("suffixArray single") {
    assertEquals(suffixArray(Array(0)).toSeq, Seq(0))
    assertEquals(suffixArray(Array(-1)).toSeq, Seq(0))
    assertEquals(suffixArray(Array(1)).toSeq, Seq(0))
    assertEquals(suffixArray(Array(Int.MinValue)).toSeq, Seq(0))
    assertEquals(suffixArray(Array(Int.MaxValue)).toSeq, Seq(0))
  }

  test("suffixArray SALCP naive") {
    for (n <- 1 to 5) {
      val m = 1 << (n * 2) // 4^n
      for (f <- 0 until m) {
        val s = new Array[Int](n)
        val maxC = {
          var g = f
          var maxC = 0
          for (i <- 0 until n) {
            s(i) = g % 4
            maxC = maxC.max(s(i))
            g /= 4
          }
          maxC
        }

        val sa = suffixArrayNaive(s)

        assertEquals(suffixArray(s).toSeq, sa.toSeq)
        assertEquals(suffixArray(s, maxC).toSeq, sa.toSeq)
        assertEquals(lcpArray(s, sa).toSeq, lcpArrayNaive(s, sa).toSeq)
      }
    }
    for (n <- 1 to 10) {
      val m = 1 << n
      for (f <- 0 until m) {
        val s = new Array[Int](n)
        val maxC = {
          var g = f
          var maxC = 0
          for (i <- 0 until n) {
            s(i) = g % 2
            maxC = maxC.max(s(i))
            g /= 2
          }
          maxC
        }

        val sa = suffixArrayNaive(s)

        assertEquals(suffixArray(s).toSeq, sa.toSeq)
        assertEquals(suffixArray(s, maxC).toSeq, sa.toSeq)
        assertEquals(lcpArray(s, sa).toSeq, lcpArrayNaive(s, sa).toSeq)
      }
    }
  }

  test("suffixArray all A test") {
    for (n <- 1 to 100) {
      val s = Array.fill(n)(10)
      assertEquals(suffixArray(s).toSeq, suffixArrayNaive(s).toSeq)
      assertEquals(suffixArray(s, 10).toSeq, suffixArrayNaive(s).toSeq)
      assertEquals(suffixArray(s, 12).toSeq, suffixArrayNaive(s).toSeq)
    }
  }

  test("suffixArray all AB test") {
    for (n <- 1 to 100) {
      val s = Array.tabulate(n)(_ % 2)
      assertEquals(suffixArray(s).toSeq, suffixArrayNaive(s).toSeq)
      assertEquals(suffixArray(s, 3).toSeq, suffixArrayNaive(s).toSeq)
    }
    for (n <- 1 to 100) {
      val s = Array.tabulate(n)(i => 1 - (i % 2))
      assertEquals(suffixArray(s).toSeq, suffixArrayNaive(s).toSeq)
      assertEquals(suffixArray(s, 3).toSeq, suffixArrayNaive(s).toSeq)
    }
  }

  test("suffixArray string") {
    val s = "missisippi"

    val sa = suffixArray(s)

    // format: off
    val answer = IndexedSeq(
      "i",           // 9
      "ippi",        // 6
      "isippi",      // 4
      "issisippi",   // 1
      "missisippi",  // 0
      "pi",          // 8
      "ppi",         // 7
      "sippi",       // 5
      "sisippi",     // 3
      "ssisippi",    // 2
    )
    // format: on

    assertEquals(sa.length, answer.size)

    sa.indices.foreach(i => {
      assertEquals(s.substring(sa(i)), answer(i))
    })
  }

  test("lcpArray") {
    val s = "aab"
    val sa = suffixArray(s)
    assertEquals(sa.toSeq, Seq(0, 1, 2))

    val lcp = lcpArray(s, sa).toSeq
    assertEquals(lcp, Seq(1, 0))

    assertEquals(lcpArray(Array(0, 0, 1), sa).toSeq, lcp)
    assertEquals(lcpArray(Array(-100, -100, 100), sa).toSeq, lcp)
    assertEquals(lcpArray(Array(Byte.MinValue, Byte.MinValue, Byte.MaxValue), sa).toSeq, lcp)
    assertEquals(lcpArray(Array(Int.MinValue, Int.MinValue, Int.MaxValue), sa).toSeq, lcp)
    assertEquals(lcpArray(Array(Long.MinValue, Long.MinValue, Long.MaxValue), sa).toSeq, lcp)
  }

  private def zAlgorithmNaive[T](s: Array[T]): Array[Int] = {
    val n = s.length
    val z = new Array[Int](n)
    for (i <- 0 until n) {
      while (i + z(i) < n && s(z(i)) == s(i + z(i))) {
        z(i) += 1
      }
    }
    z
  }

  test("zAlgorithm empty") {
    assertEquals(zAlgorithm("").toSeq, Seq.empty[Int])
    assertEquals(zAlgorithm(Array.empty[Int]).toSeq, Seq.empty[Int])
  }

  test("zAlgorithm simple") {
    {
      val str = "abab"
      val lcp = zAlgorithm(str)

      assertEquals(lcp.toSeq, Seq(4, 0, 2, 0))
    }
    {
      val str = "abracadabra"
      val lcp = zAlgorithm(str)

      assertEquals(lcp.toSeq, Seq(11, 0, 0, 1, 0, 1, 0, 4, 0, 0, 1))
    }
    {
      val str = "ababababa"
      val lcp = zAlgorithm(str)

      assertEquals(lcp.toSeq, Seq(9, 0, 7, 0, 5, 0, 3, 0, 1))
    }

    assertEquals(zAlgorithm(Array(1, 10, 1, 10)).toSeq, Seq(4, 0, 2, 0))
    assertEquals(zAlgorithm(Array(0, 0, 0, 0, 0, 0, 0)).toSeq, zAlgorithmNaive(Array(0, 0, 0, 0, 0, 0, 0)).toSeq)
  }

  test("zAlgorithm naive") {
    for (n <- 1 to 6) {
      val m = 1 << (n * 2) // 4^n
      for (f <- 0 until m) {
        val s = new Array[Int](n)
        var g = f
        for (i <- 0 until n) {
          s(i) = g % 4
          g /= 4
        }
        assertEquals(zAlgorithm(s).toSeq, zAlgorithmNaive(s).toSeq)
      }
    }
    for (n <- 1 to 10) {
      val m = 1 << n
      for (f <- 0 until m) {
        val s = new Array[Int](n)
        var g = f
        for (i <- 0 until n) {
          s(i) = g % 2
          g /= 2
        }
        assertEquals(zAlgorithm(s).toSeq, zAlgorithmNaive(s).toSeq)
      }
    }
  }

}
