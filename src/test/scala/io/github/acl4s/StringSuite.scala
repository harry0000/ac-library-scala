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
    assertEquals(suffixArrayArbitrary(Array.empty[Int]).toSeq, Seq.empty[Int])
    assertEquals(suffixArrayArbitrary(Array.empty[Long]).toSeq, Seq.empty[Int])
  }

  test("suffixArray single") {
    assertEquals(suffixArrayArbitrary(Array(0)).toSeq, Seq(0))
    assertEquals(suffixArrayArbitrary(Array(-1)).toSeq, Seq(0))
    assertEquals(suffixArrayArbitrary(Array(1)).toSeq, Seq(0))
    assertEquals(suffixArrayArbitrary(Array(Int.MinValue)).toSeq, Seq(0))
    assertEquals(suffixArrayArbitrary(Array(Int.MaxValue)).toSeq, Seq(0))
  }

  test("suffixArray SALCP naive") {
    for (n <- 1 to 5) {
      val m = (0 until n).foldLeft(1) { case (acc, _) => acc * 4 }
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

        assertEquals(suffixArrayArbitrary(s).toSeq, sa.toSeq)
        assertEquals(suffixArray(s, maxC).toSeq, sa.toSeq)
        assertEquals(lcpArrayArbitrary(s, sa).toSeq, lcpArrayNaive(s, sa).toSeq)
      }
    }
    for (n <- 1 to 10) {
      val m = (0 until n).foldLeft(1) { case (acc, _) => acc * 2 }
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

        assertEquals(suffixArrayArbitrary(s).toSeq, sa.toSeq)
        assertEquals(suffixArray(s, maxC).toSeq, sa.toSeq)
        assertEquals(lcpArrayArbitrary(s, sa).toSeq, lcpArrayNaive(s, sa).toSeq)
      }
    }
  }

  test("suffixArray all A test") {
    for (n <- 1 to 100) {
      val s = Array.fill(n)(10)
      assertEquals(suffixArrayArbitrary(s).toSeq, suffixArrayNaive(s).toSeq)
      assertEquals(suffixArray(s, 10).toSeq, suffixArrayNaive(s).toSeq)
      assertEquals(suffixArray(s, 12).toSeq, suffixArrayNaive(s).toSeq)
    }
  }

  test("suffixArray all AB test") {
    for (n <- 1 to 100) {
      val s = Array.tabulate(n)(_ % 2)
      assertEquals(suffixArrayArbitrary(s).toSeq, suffixArrayNaive(s).toSeq)
      assertEquals(suffixArray(s, 3).toSeq, suffixArrayNaive(s).toSeq)
    }
    for (n <- 1 to 100) {
      val s = Array.tabulate(n)(i => 1 - (i % 2))
      assertEquals(suffixArrayArbitrary(s).toSeq, suffixArrayNaive(s).toSeq)
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

    assertEquals(lcpArrayArbitrary(Array(0, 0, 1), sa).toSeq, lcp)
    assertEquals(lcpArrayArbitrary(Array(-100, -100, 100), sa).toSeq, lcp)
    assertEquals(lcpArrayArbitrary(Array(Byte.MinValue, Byte.MinValue, Byte.MaxValue), sa).toSeq, lcp)
    assertEquals(lcpArrayArbitrary(Array(Int.MinValue, Int.MinValue, Int.MaxValue), sa).toSeq, lcp)
    assertEquals(lcpArrayArbitrary(Array(Long.MinValue, Long.MinValue, Long.MaxValue), sa).toSeq, lcp)
  }

  test("zAlgorithm") {
    {
      val str = "abracadabra";
      val lcp = zAlgorithm(str)

      assertEquals(lcp.toSeq, Seq(11, 0, 0, 1, 0, 1, 0, 4, 0, 0, 1))
    }
    {
      val str = "ababababa"
      val lcp = zAlgorithm(str)

      assertEquals(lcp.toSeq, Seq(9, 0, 7, 0, 5, 0, 3, 0, 1))
    }
  }

}
