package example

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_k]]
 */
class K_RangeAffineRangeSumSuite extends BaseFunSuite {

  test("Sample Input 1") {
    val input =
      """5 7
        |1 2 3 4 5
        |1 0 5
        |0 2 4 100 101
        |1 0 3
        |0 1 3 102 103
        |1 2 5
        |0 2 5 104 105
        |1 0 5
        |""".stripMargin

    val output =
      """15
        |404
        |41511
        |4317767""".stripMargin

    assertEquals(run(K_RangeAffineRangeSum.main, input).lines().toList, output.lines().toList)
  }

}
