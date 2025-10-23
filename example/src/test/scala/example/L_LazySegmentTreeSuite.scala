package example

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_l]]
 */
class L_LazySegmentTreeSuite extends BaseFunSuite {

  test("Sample Input 1") {
    val input =
      """5 5
        |0 1 0 0 1
        |2 1 5
        |1 3 4
        |2 2 5
        |1 1 3
        |2 1 2""".stripMargin

    val output =
      """2
        |0
        |1""".stripMargin

    assertEquals(run(L_LazySegmentTree.main, input).lines().toList, output.lines().toList)
  }

}
