package io.github.acl4s

class MathSuite extends munit.FunSuite {

  /**
   * @see https://atcoder.jp/contests/practice2/tasks/practice2_c
   */
  test("AtCoder Library Practice Contest C - Floor Sum") {
    // format: off
    assertEquals(floorSum(n = 4L,          m = 10L,         a = 6L,         b = 3L        ), 3L                 )
    assertEquals(floorSum(n = 6L,          m = 5L,          a = 4L,         b = 3L        ), 13L                )
    assertEquals(floorSum(n = 1L,          m = 1L,          a = 0L,         b = 0L        ), 0L                 )
    assertEquals(floorSum(n = 31415L,      m = 92653L,      a = 58979L,     b = 32384L    ), 314095480L         )
    assertEquals(floorSum(n = 1000000000L, m = 1000000000L, a = 999999999L, b = 999999999L), 499999999500000000L)
    // format: on
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
